library(tidyverse)
library(nflreadr) 
library(brms)
library(lme4)  
options(brms.backend = "cmdstanr") 

# read nfl data from previous project 
data = read_csv("nfl_seasonal_data/standings.csv") 

# clean team names 
data$Tm <- gsub("[\\*\\+]", "", data$Tm)
data$Tm[data$Tm == "Oakland Raiders"] <- "Las Vegas Raiders"
data$Tm[data$Tm == "St. Louis Rams"] <- "Los Angeles Rams"
data$Tm[data$Tm == "San Diego Chargers"] <- "Los Angeles Chargers"
data$Tm[data$Tm == "Washington Redskins"] <- "Washington Commanders"
data$Tm[data$Tm == "Washington Football Team"] <- "Washington Commanders"

# get prior year statistics 
data$prior_wins <- NA
data$prior_losses <- NA
data$prior_SoS <- NA
data$prior_PD <- NA 

for( j in 1:nrow(data) ) {
    ii <- which( data$Tm == data$Tm[j] & data$year == data$year[j] - 1 )
    if( length(ii) == 1 ){
        data$prior_wins[j] <- data$W[ii]
        data$prior_losses[j] <- data$L[ii]
        data$prior_SoS[j] <- data$SoS[ii]
        data$prior_PD[j] <- data$PD[ii]
    }
}

# 2016 onwards 
data = data %>% 
  filter(year >= 2016) %>% 
  mutate(
    prior_PD = as.vector(scale(prior_PD)), 
    Tm = clean_team_abbrs(Tm)
  )

# get contracts from 2016-24 
contracts = load_contracts() %>% 
  filter(year_signed %in% c(2016:2024)) %>%
  mutate(team = clean_team_abbrs(team)) 

# get rosters from 2016-2024 
rosters = load_rosters(seasons = c(2016:2024)) 

# draft picks 
draft_picks = load_draft_picks(seasons = c(2016:2024)) %>%
  mutate(team = clean_team_abbrs(team))

# change some outdated team names 
contracts$team[contracts$team == 'OAK'] = 'LV' 
contracts$team[contracts$team == 'SD'] = 'LAC' 
rosters$team[rosters$team == 'OAK'] = 'LV'
rosters$team[rosters$team == 'SD'] = 'LAC'

# how to determine if player was newly signed: 
# get end-of-season roster for each team at year Y 
# get end-of-season roster for each team at year Y + 1 
# do anti-join to get list of players who were added between seasons 
# get list of players who signed contracts with each team at year Y + 1 
# who ever is included in both lists signed as either a free agent or draft and were NOT traded 

# let's this out with the kansas city chiefs (KC) 
kc_23_roster = rosters %>% 
  filter(team == 'KC', season == 2023) 

kc_24_roster = rosters %>% 
  filter(team == 'KC', season == 2024)

# all players from 2024 roster NOT included in 2023 roster
new_kc = anti_join(kc_24_roster, kc_23_roster, by = "gsis_id")

# contracts offered by KC in 2024 
kc_24_contracts = contracts %>% 
  filter(team_abbr == 'KC', year_signed == '2024') %>%
  distinct(gsis_id, .keep_all = TRUE) %>% 
  select(player, gsis_id, team_abbr, apy, draft_round, draft_year, draft_team) 

# players newly on 2024 roster AND with contract info 
# this is a combo of free agents + draft picks 
kc_24_signings = inner_join(new_kc, kc_24_contracts, by = "gsis_id")

# now let's write a function to handle all teams, all years 
years = c(2015:2024) 

get_player_signings = function(rosters, contracts, years) {

  results = tibble() 

  for (i in 1:(length(years) - 1)) {
    base_year = years[i] 
    next_year = years[i+1] 

    base_roster = rosters %>% 
      filter(season == base_year) %>% 
      select(season, team, position, gsis_id, full_name)

    next_roster = rosters %>% 
      filter(season == next_year) %>% 
      select(season, team, position, gsis_id, full_name) 

    new_players = anti_join(next_roster, base_roster, by = c("gsis_id", "team")) 

    new_contracts = contracts %>% 
      filter(year_signed == next_year) %>% 
      select(gsis_id, team, apy, draft_round, draft_year, draft_team) 

    entry = inner_join(new_players, new_contracts, by = c("gsis_id", "team"))
    results = bind_rows(results, entry)
  }
  return(results)
}

all_signings = get_player_signings(rosters, contracts, years)

signings_by_team = all_signings %>% 
  group_by(team, season) %>% 
  summarize(
    total_apy = sum(apy), 
    max_apy = max(apy)
  )

# draft picks 
top_picks = draft_picks %>% 
  group_by(team, season) %>% 
  filter(pick == min(pick)) %>% 
  ungroup() %>% 
  select(team, season, pick, position, category, age)

# join on signing + draft data 
data = data %>% 
  left_join(
    signings_by_team, by = join_by(year == season, Tm == team)
  ) %>% 
  left_join(
    top_picks, by = join_by(year == season, Tm == team)
  )

# getting what we need 
model_df = data %>% 
  select(
    year, Tm, W, L, prior_PD, total_apy, max_apy, pick, category 
  ) %>% 
  mutate(
    G = W + L, 
    total_apy = as.vector(scale(total_apy)), 
    max_apy = as.vector(scale(max_apy)), 
    pick = as.vector(scale(pick)), 
    category = as.factor(category)
  )

train = model_df %>% filter(between(year, 2016, 2023)) 
test = model_df %>% filter(year == 2024)

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
mean(abs(test$W - bb_wins)) # 2.69! 

saveRDS(fit_bb, "beta_binomial_mod.rds") 
