library(tidyverse)
library(nflreadr) 

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

# 2016 onwards, since there's no prior PD for 2015 
data = data %>% 
  filter(year >= 2016) %>% 
  mutate(Tm = clean_team_abbrs(Tm)) 

# get contracts from 2016-24 
contracts = load_contracts() %>% 
  filter(year_signed %in% c(2016:2024)) %>%
  mutate(team = clean_team_abbrs(team)) 

# get rosters from 2016-2024 
rosters = load_rosters(seasons = c(2016:2024)) 

# change some outdated team names 
contracts$team[contracts$team == 'OAK'] = 'LV' 
contracts$team[contracts$team == 'SD'] = 'LAC' 
rosters$team[rosters$team == 'OAK'] = 'LV'
rosters$team[rosters$team == 'SD'] = 'LAC'

# a function that returns free agent signings for each team from 2016-2024 
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

# group by team and season, then find the total average contract value and maximum average contract value 
signings_by_team = all_signings %>% 
  group_by(team, season) %>% 
  summarize(
    total_apy = sum(apy), 
    max_apy = max(apy)
  )

# draft picks from 2016 to 2024 
draft_picks = load_draft_picks(seasons = c(2016:2024)) %>%
  mutate(team = clean_team_abbrs(team))

# group by team and season, then filter for first-round picks only 
top_picks = draft_picks %>% 
  group_by(team, season) %>% 
  filter(pick == min(pick)) %>% 
  ungroup() %>% 
  select(team, season, pick, position, category, age)

# merge signing + draft information with main data 
data = data %>% 
  left_join(
    signings_by_team, by = join_by(year == season, Tm == team)
  ) %>% 
  left_join(
    top_picks, by = join_by(year == season, Tm == team)
  )

# only select the columns that we need 
# we also scale and center the numerical covariates for stability 
# we convert the category variable into a factor 
model_df = data %>% 
  select(
    year, Tm, W, L, prior_PD, total_apy, max_apy, pick, category 
  ) %>% 
  mutate(
    G = W + L, 
    total_apy = as.vector(scale(total_apy)), 
    prior_PD = as.vector(scale(prior_PD)), 
    max_apy = as.vector(scale(max_apy)), 
    pick = as.vector(scale(pick)), 
    category = as.factor(category)
  )

# train on 2016-23, test on 2024 
train = model_df %>% filter(between(year, 2016, 2023)) 
test = model_df %>% filter(year == 2024)

# save the data 
saveRDS(train, "training_data.rds") 
saveRDS(test, "test_data.rds")


##############################################
# additional visuals for EDA/report purposes
##############################################

# dist'n of positions of first round draft picks
pos_counts <- table(data$position)
pos_counts <- sort(pos_counts, decreasing = TRUE)

pos_df <- data.frame(
  position = names(pos_counts),
  count = as.vector(pos_counts),
  row.names = NULL
)

ggplot(pos_df, aes(x = reorder(position, -count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Distribution of Drafted Positions (First-Round Picks)",
    x = "Position",
    y = "Number of Draft Selections"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# here we see that WR and QB positions are most popular/overrepresented



#dist'n of free agent contract dollar value
ggplot(signings_by_team, aes(x = total_apy)) +
  geom_histogram(bins = 30, fill = "seagreen", color = "white") +
  labs(
    title = "Distribution of Total Free-Agent Spending (2016–2024)",
    x = "Total Contract Value (APY Sum)",
    y = "Number of Team-Seasons"
  ) +
  theme_minimal()

# trying to figure out if each team only gets 1 first round draft pick per season 
print(n=40, data[data$year == 2024, c("year", "Tm", "position")])

table(paste(data$year, data$Tm)) |> sort(decreasing = TRUE) # no duplicates for any team is good

