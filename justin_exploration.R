library(tidyverse)
library(nflreadr) 
library(brms)
options(brms.backend = "cmdstanr") 

# read nfl data from previous project 
data = read_csv("nfl_seasonal_data/standings.csv") 

data$Tm <- gsub("[\\*\\+]", "", data$Tm)
data$Tm[data$Tm == "Oakland Raiders"] <- "Las Vegas Raiders"
data$Tm[data$Tm == "St. Louis Rams"] <- "Los Angeles Rams"
data$Tm[data$Tm == "San Diego Chargers"] <- "Los Angeles Chargers"
data$Tm[data$Tm == "Washington Redskins"] <- "Washington Commanders"
data$Tm[data$Tm == "Washington Football Team"] <- "Washington Commanders"

data$prior_wins <- NA
data$prior_losses <- NA
data$prior_SoS <- NA

for( j in 1:nrow(data) ) {
    ii <- which( data$Tm == data$Tm[j] & data$year == data$year[j] - 1 )
    if( length(ii) == 1 ){
        data$prior_wins[j] <- data$W[ii]
        data$prior_losses[j] <- data$L[ii]
        data$prior_SoS[j] <- data$SoS[ii]
    }
}

data = data %>% 
  mutate(
    team_abbr = clean_team_abbrs(Tm)
  ) 

data21_24 = data %>% 
  filter(between(year, 2021, 2024))

draft_picks = load_draft_picks(seasons = c(2021:2024)) 

data21_24 = data21_24 %>% 
  left_join(
    draft_picks, by = join_by(year == season, team_abbr == team), 
    relationship = "many-to-many"
  )

# model: 2024 win total depends on the following: 
# 2024 highest pick number + position 
# 2023 highest pick number + position 
# 2023, 2022, 2021 point differential
# ARMA correlation structure 

model_df = data21_24 %>% 
  select(
    year, Tm, W, L, pick, position, age
  ) %>% 
  drop_na() %>% 
  group_by(year, Tm) %>% 
  filter(pick == min(pick)) %>% 
  ungroup() %>% 
  mutate(
    position_cat = case_when(
      position == 'QB' ~ 'QB', 
      position == 'WR' ~ 'WR', 
      position == 'OL' ~ 'OL', 
      position == 'DB' ~ 'DB', 
      .default = 'Other'
    ), 
    position_cat = factor(position_cat, levels = c('QB','WR','OL','DB','Other')), 
    G = W + L 
  )

priors = get_prior(
  bf(W | trials(G) ~ pick + position_cat + age + arma(time = year, gr = Tm, p = 1, q = 1)), 
  data = model_df, family = beta_binomial(link = "logit", link_phi = "log") 
)

priors = priors %>% 
  mutate(
    prior = case_when(
      class == 'Intercept' ~ 'normal(0.5, 0.1)', 
      class %in% c('ar','ma','b') ~ 'normal(0, 2)', 
      class %in% c('phi','sderr') ~ 'exponential(1)'
    )
  )

# takes more than 10 minutes... 
fit = brm(
  bf(W | trials(G) ~ pick + position_cat + age + arma(time = year, gr = Tm, p = 1, q = 1, cov = TRUE)), 
  data = model_df, family = beta_binomial(link = "logit", link_phi = "log"), 
  cores = 4, chains = 4, 
  warmup = 1000, iter = 2000, seed = 76
)

summary(fit) 
pp_check(fit, ndraws=100)

# fixes 
# instead of pick, use round number (1 or 2) 
# different prior for intercept 
# mixture or two distributions 
# ARMA may not be necessary 

mix = mixture(binomial, binomial)
mix_prior = get_prior(
  bf(
    W | trials(G) ~ 1, 
    theta1 = 1, theta2 = 4 
  ), 
  data = model_df, family = mix
)

fit_mix = brm(
  bf(
    W | trials(G) ~ 1, 
    theta1 = 1, theta2 = 2
  ), 
  data = model_df, family = mix, 
  prior = c(
    prior(student_t(1, -1, 0.5), Intercept, dpar = mu1), 
    prior(student_t(1, 0, 0.5), Intercept, dpar = mu2)
  ), 
  cores = 4, chains = 4, 
  warmup = 1000, iter = 2500, seed = 76,
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)

# because we're using a logit link function 
# mu = -1 assumes winning pct. of around 27% (tanking teams)
# mu = 0 assumes winning pct. of around 50% (regular teams)
# assume that about 1/3 teams tank and 2/3 teams try to be competitive 
# this corresponds to theta1 = 1, theta2 = 2 

summary(fit_mix)
pp_check(fit_mix, ndraws = 100)
