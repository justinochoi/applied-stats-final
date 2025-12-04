library(tidyverse)
library(brms)
library(lme4)  
options(brms.backend = "cmdstanr") 

train = readRDS("training_data.rds") 
test = readRDS("test_data.rds") 

# baseline model: binomial model with prior PD and team randeff 
baseline = glmer(
  cbind(W, L) ~ prior_PD + (1 | Tm), 
  data = train, family = binomial(link = "logit")
)
summary(baseline)

base_prob = predict(baseline, test, type = "response") 
base_wins = 17 * base_prob 
mean(abs(test$W - base_wins)) # 2.83 
sqrt(mean((test$W - base_wins)^2)) # 3.4 

# adding signing data 
sign_lmer = glmer(
  cbind(W, L) ~ max_apy + prior_PD + pick + category + (1 | Tm), 
  data = train, family = binomial(link = "logit")
)
summary(sign_lmer) 

# compare the two models 
# adding signing information seems to be useful 
anova(baseline, sign_lmer)

sign_prob = predict(sign_lmer, test, type = "response") 
sign_wins = 17 * sign_prob
mean(abs(test$W - sign_wins)) # 2.79 - baseline defeated! 
sqrt(mean((test$W - sign_wins)^2)) # 3.46 - but RMSE is worse 

# beta-binomial using brms  
bb_prior = get_prior(
  bf(
    W | trials(G) ~ prior_PD + category + pick + max_apy + (1 | Tm)
  ), 
  data = train, family = beta_binomial 
)

bb_prior = bb_prior %>% 
  mutate(
    prior = case_when(
      class == 'b' ~ 'normal(0,2)', 
      class == 'Intercept' ~ 'normal(0, 0.5)', 
      class == 'sd' ~ 'exponential(1)', 
      class == 'phi' ~ 'exponential(0.1)'
    )
  )

# WARNING: do not run this model by yourself!! it will take ~ 20 minutes to evalute 
# instead load the saved model directly: 
fit_bb = readRDS("beta_binomial_mod.rds") 

#fit_bb = brm(
#  bf(
#    W | trials(G) ~ prior_PD + category + pick + max_apy + (1 | Tm) 
#  ), 
#  data = train, family = beta_binomial, 
#  prior = bb_prior, 
#  cores = 4, chains = 4, 
#  warmup = 1000, iter = 2500, seed = 76,
#  control = list(adapt_delta = 0.95, max_treedepth = 15)
#)

summary(fit_bb) 
pp_check(fit_bb, ndraws=200) 

bb_wins = colMeans(posterior_epred(fit_bb, newdata = test))
mean(abs(test$W - bb_wins)) # 2.69! best one yet 
sqrt(mean((test$W - bb_wins)^2)) # 3.43 - very slightly worse than baseline but closest 
cor(test$W, bb_wins) # 0.377 

# hail mary: gaussian process for modeling varying team strength over time 
gp_prior = get_prior(
  bf(
    W | trials(G) ~ gp(year, by = Tm, k = 5) + (1 | Tm)
  ), 
  data = train, family = binomial 
)

gp_prior = gp_prior %>% 
  mutate(
    prior = case_when(
      class == 'b' ~ 'normal(0,2)', 
      class == 'Intercept' ~ 'normal(0, 0.5)', 
      class == 'sd' ~ 'exponential(1)', 
      TRUE ~ prior
    )
  )

fit_gp = brm(
  bf(
    W | trials(G) ~ gp(year, by = Tm, k = 5) + (1 | Tm)
  ), 
  data = train, family = binomial, 
  prior = gp_prior, 
  cores = 4, chains = 4, 
  warmup = 1000, iter = 2500, seed = 76,
  control = list(adapt_delta = 0.95, max_treedepth = 15)
)

summary(fit_gp)
pp_check(fit_gp, ndraws = 200)

# the GP model needs past data as well 
gp_wins_all = colMeans(posterior_epred(fit_gp, newdata = bind_rows(train, test)))
# these are the predictions for 2024 
gp_wins = gp_wins_all[257:288]
mean(abs(test$W - gp_wins)) # 2.74 
sqrt(mean((test$W - gp_wins)^2)) # 3.25! - this is actual the best model 

gp_results = tibble(
  team = test$Tm, 
  preds = gp_wins[257:288], 
  actual = test$W
)

ggplot(gp_results, aes(x=preds, y=actual)) + 
  geom_point() + 
  geom_abline(slope = 1, linetype = "dashed", color = "red")

saveRDS(fit_gp, "gp_mod.rds") 
