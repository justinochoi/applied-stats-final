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


# let's try out a mixture of two binomials 
mix = mixture(binomial, binomial)
mix_prior = get_prior(
  bf(
    W | trials(G) ~ 1, 
    mu1 ~ max_apy + pick + category + prior_PD + (1 | Tm), 
    mu2 ~ max_apy + pick + category + prior_PD + (1 | Tm), 
    theta1 = 1, theta2 = 2 
  ), 
  data = train, family = mix
)

mix_prior = mix_prior %>% 
  mutate(
    prior = case_when(
      class == 'Intercept' & dpar == 'mu1' ~ 'normal(-1, 0.5)', 
      class == 'Intercept' & dpar == 'mu2' ~ 'normal(1, 0.5)', 
      class == 'b' ~ 'normal(0, 2)', 
      class == 'sd' ~ 'exponential(1)', 
      class == 'phi1' ~ 'exponential(0.1)', 
      class == 'phi2' ~ 'exponential(0.1)'
    )
  )

# because we're using a logit link function 
# mu = -1 assumes winning pct. of around 27% (tanking teams)
# mu = 0 assumes winning pct. of around 50% (regular teams)
# assume that about 1/3 teams tank and 2/3 teams try to be competitive 
# this corresponds to theta1 = 1, theta2 = 2 

fit_mix = brm(
  bf(
    W | trials(G) ~ 1, 
    mu1 ~ max_apy + pick + category + prior_PD + (1 | Tm), 
    mu2 ~ max_apy + pick + category + prior_PD + (1 | Tm), 
    theta1 = 1, theta2 = 2 
  ), 
  data = train, family = mix, 
  prior = mix_prior, 
  cores = 4, chains = 4, 
  warmup = 1000, iter = 3000, seed = 76,
  control = list(adapt_delta = 0.95, max_treedepth = 15)
)

summary(fit_mix) 
pp_check(fit_mix, ndraws=200)

mix_wins = colMeans(posterior_epred(fit_mix, newdata = test))
mean(abs(test$W - mix_wins)) # 2.96 - overfitting 

# no mixture, but beta binomial family 
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
      class == 'Intercept' ~ 'normal(1, 0.5)', 
      class == 'sd' ~ 'exponential(1)', 
      class == 'phi' ~ 'exponential(0.1)'
    )
  )

fit_bb = brm(
  bf(
    W | trials(G) ~ prior_PD + category + pick + max_apy + (1 | Tm) 
  ), 
  data = train, family = beta_binomial, 
  prior = bb_prior, 
  cores = 4, chains = 4, 
  warmup = 1000, iter = 2500, seed = 76,
  control = list(adapt_delta = 0.95, max_treedepth = 15)
)

summary(fit_bb) 
pp_check(fit_bb, ndraws=200) 

bb_wins = colMeans(posterior_epred(fit_bb, newdata = test))
mean(abs(test$W - bb_wins)) # 2.69! best one yet 

# plotting the predictions 
results = tibble(
  team = test$Tm, 
  preds = bb_wins, 
  actual = test$W 
) 

plot(base_wins, test$W)
cor(sign_wins, test$W) 
cor(mix_wins, test$W) 
cor(results$preds, results$actual)

sqrt(mean((base_wins - test$W)^2))
sqrt(mean((sign_wins - test$W)^2)) 
sqrt(mean((bb_wins - test$W)^2))

# one of the biggest misses is the tennessee titans 
# they signed calvin ridley to 4 yr, $92 mil deal 

ggplot(results, aes(x=preds, y=actual)) + 
  geom_point() + 
  geom_abline(slope = 1, linetype = "dashed", color = "red") + 
  coord_fixed() 

saveRDS(fit_bb, "beta_binomial_mod.rds") 