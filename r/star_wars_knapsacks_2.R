# Description --------------------------------------------------------------------------------------
# Simulate adversarial knapsack game with a variety of settings.

# Setup --------------------------------------------------------------------------------------------
# Libraries
# For data manipulation
library(dplyr)
library(purrr)
library(tidyr)
# For kurtosis and skew functions
library(e1071)
# RcppAlgos::comboGeneral is ~80x faster than utils::combn in my quick test, it also seems to beat
# data.table::CJ. It can also run in parallel, but this doesn't seem to help on Windows. Might be
# worth trying on the Econ linux machines?
library(RcppAlgos)
# For compiling functions to bytecode, this usually nets a tiny speedup
library(compiler)
data(starwars)

# Import knapsack functions
source("r/knapsack_functions.R")

# Simulation settings
# number of draws from each items distribution, used to estimated utility of the item 
n_draws <- 10000

# a ------------------------------------------------------------------------------------------------
# 2 uniform items (same expectation, different variance)
n_players <- 2
knap_size <- 1
a <- data_frame(
  draws = list(
    runif(n_draws, 1, 2),
    runif(n_draws, 0, 3)
  ),
  price = 1
) %>% 
  mutate(item = sample(unlist(starwars$starships), n(), replace = F)) %>% 
  form_knapsacks(knap_size) %>% 
  left_join(., estimate_knapsack_utility(., n_players), by = "knap_id")
a
# Lesson: Variance doesn't matter, just don't choose the same thing as your opponent

# b ------------------------------------------------------------------------------------------------
# 3 uniform items (same expectation, different variance)
n_players <- 2
knap_size <- 1
b <- data_frame(
  draws = list(
    runif(n_draws, 1, 2),
    runif(n_draws, 0, 3),
    runif(n_draws, -1, 4)
  ),
  price = 1
) %>% 
  mutate(item = sample(unlist(starwars$starships), n(), replace = F)) %>% 
  form_knapsacks(knap_size) %>% 
  left_join(., estimate_knapsack_utility(., n_players), by = "knap_id")
b

b %>%
  group_by(knap_id) %>%
  summarise_if(is.numeric, mean) %>%
  arrange(utility)

# Lesson: Variance doesn't matter, just don't choose the same thing as your opponent, this is a 
#         little easier now.

# c ------------------------------------------------------------------------------------------------
# 3 uniform items (same expectation, different variance)
n_players <- 3
knap_size <- 1
c <- data_frame(
  draws = list(
    runif(n_draws, 1, 2),
    runif(n_draws, 0, 3),
    runif(n_draws, -1, 4)
  ),
  price = 1
) %>% 
  mutate(item = sample(unlist(starwars$starships), n(), replace = F)) %>% 
  form_knapsacks(knap_size) %>% 
  left_join(., estimate_knapsack_utility(., n_players), by = "knap_id")
c

c %>%
  group_by(knap_id) %>%
  summarise_if(is.numeric, mean) %>%
  arrange(utility)

# Lesson: With 3 players, we now care more about variance

# d ------------------------------------------------------------------------------------------------
# 5 uniform items (same expectation, different variance)
n_players <- 4
knap_size <- 1
d <- data_frame(
  draws = list(
    runif(n_draws, 1, 2),
    runif(n_draws, 0, 3),
    runif(n_draws, -1, 4),
    runif(n_draws, -2, 5),
    runif(n_draws, -3, 6)
  ),
  price = 1
) %>% 
  mutate(item = sample(unlist(starwars$starships), n(), replace = F)) %>% 
  form_knapsacks(knap_size) %>% 
  left_join(., estimate_knapsack_utility(., n_players), by = "knap_id")
d

d %>%
  group_by(knap_id) %>%
  summarise_if(is.numeric, mean) %>%
  arrange(utility)

# e ------------------------------------------------------------------------------------------------
# 5 uniform items
n_players <- 3
knap_size <- 1
e <- data_frame(
  draws = list(
    runif(n_draws, 1, 2),
    runif(n_draws, 0, 3),
    runif(n_draws, -1, 4),
    runif(n_draws, -2, 5),
    runif(n_draws, -3, 6)
  ),
  price = 1
) %>% 
  mutate(item = sample(unlist(starwars$starships), n(), replace = F)) %>% 
  form_knapsacks(knap_size) %>% 
  left_join(., estimate_knapsack_utility(., n_players), by = "knap_id")
e

e %>%
  group_by(knap_id) %>%
  summarise_if(is.numeric, mean) %>%
  arrange(utility) %>% 
  ggplot(aes(utility, var)) +
  geom_point()

# Lesson: It looks like variance increaes in importance, to a point.

# e ------------------------------------------------------------------------------------------------
# 5 uniform items, but increasing knap size
n_players <- 3
knap_size <- 2
f <- data_frame(
  draws = list(
    runif(n_draws, 1, 2),
    runif(n_draws, 0, 3),
    runif(n_draws, -1, 4),
    runif(n_draws, -2, 5),
    runif(n_draws, -3, 6)
  ),
  price = 1
) %>% 
  mutate(item = sample(unlist(starwars$starships), n(), replace = F)) %>% 
  form_knapsacks(knap_size) %>% 
  left_join(., estimate_knapsack_utility(., n_players), by = "knap_id")
f

f %>%
  group_by(knap_id) %>%
  summarise_if(is.numeric, mean) %>%
  arrange(utility)

