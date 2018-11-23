# Ben Ewing on 2018-11-23

# Libraries and data
library(dplyr)
library(purrr)
library(tidyr)
library(e1071)
data(starwars)

# simulation settings
n_rounds <- 1000

# players, each with a name, budget, strat, and params for that strat
players <- data_frame(
  player = sample(starwars$name, 2, replace = F),
  budget = 5,
  strat = "sample",
  strat_params = list(c(size = 1))
)

# items, each with a name, n draws from the distribution, and price
items <- data_frame(
  item = sample(unlist(starwars$starships), 3, replace = F),
  draws = list(rnorm(n_rounds, 5, 1), rgamma(n_rounds, 2), rpois(n_rounds, 2)),
  price = 1
)

# Calculate feasible knapsacks, we can consider knapsacks of size 1:n, n, etc.
# What can the wealthiest player afford?
# n_item_bound <- 1:min(max(players$budget)/(sum(items$price)/nrow(items)), length(unique(items$item)))
# Arbitrary
n_item_bound <- 2
knapsacks <- map(n_item_bound, ~ combn(items$item, m = .x, simplify = F)) %>% 
  unlist(recursive = F) %>% 
  map_df( function(knap) {
    total <- sum(items$price[items$item %in% knap])
    draws <- reduce(items$draws[items$item %in% knap], `+`)
    data_frame(
      items = paste0(knap, collapse = ";"), 
      n_items = length(knap), 
      total_price = total,
      knap_draws = list(draws),
      mean = mean(draws), sd = sd(draws), var = var(draws),
      skewness = skewness(draws), kurtosis = kurtosis(draws)
    )
  }) %>% 
  mutate(knap_id = 1:n())
knapsacks_reduced <- knapsacks %>% 
  select(knap_id, knap_draws) %>% 
  unnest() %>% 
  group_by(knap_id) %>% 
  mutate(round = 1:n()) %>% 
  ungroup() %>% 
  group_by(round)
knapsacks <- knapsacks %>% select(-knap_draws)

# Calculate how often each knapsack will win when faced with n other knapsacks, where n is the 
# number of other players.
# i.e. to win in a three player game, a knapsack must take a value greater than that of both other
# players.
combn(knapsacks$knap_id, m = nrow(players), simplify = F) %>% 
  map_df(function(matchup) {
    knapsacks_reduced %>% 
      filter(knap_id %in% matchup) %>% 
      summarise(winner = knap_id[max(knap_draws) == knap_draws],
                matchup = paste0(matchup, collapse = ""))
  }) %>% 
  group_by(matchup, winner) %>% 
  summarise(n = n()) %>%
  mutate(win_prop = n/sum(n),
         utility = ((n()-1)*win_prop - 1*(1-win_prop))) %>% 
  rowwise() %>% 
  # make matchup directional
  mutate(matchup = paste0(winner, gsub(winner, "", matchup), collapse = "")) %>% 
  ungroup() %>% 
  select(-n) %>% 
  rename(knap_id = winner) %>% 
  left_join(knapsacks, by = "knap_id")

# For each player, choose a knapsack based on their strategy


