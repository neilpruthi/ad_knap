# Description --------------------------------------------------------------------------------------
# Functions to simulate adversarial knapsack game. This version:
# drops the dedicated player dataframe, it's an unnecesarry abstraction
# rewrites most operations as functions
# tries to optimize, generally by moving from tidyverse to base R.
# Ben Ewing on 2018-11-25

# Setup --------------------------------------------------------------------------------------------
# Libraries
# For data manipulation
require(dplyr)
require(purrr)
require(tidyr)
# For kurtosis and skew functions
require(e1071)
# RcppAlgos::comboGeneral is ~80x faster than utils::combn in my quick test, it also seems to beat
# data.table::CJ. It can also run in parallel, but this doesn't seem to help on Windows. Might be
# worth trying on the Econ linux machines?
require(RcppAlgos)
# For compiling functions to bytecode, this usually nets a tiny speedup
require(compiler)
data(starwars)

# Functions ----------------------------------------------------------------------------------------

#' Takes a dataframe of items, and knapsack sizes (must be a scalar), and returns all 
#' possible knapsacks, as well as some descriptive information about each knapsack.
form_knapsacks <- function(items, size) {
  comboGeneral(items$item, m = size) %>% 
    # purrr doesn't really like matrices, but apply is great.
    apply(1, function(knap) {
      total <- sum(items$price[items$item %in% knap])
      draws <- reduce(items$draws[items$item %in% knap], `+`)
      data_frame(
        items = paste0(knap, collapse = ";;"), 
        n_items = length(knap), 
        total_price = total,
        knap_draws = list(draws),
        mean = mean(draws), sd = sd(draws), var = var(draws),
        skewness = skewness(draws), kurtosis = kurtosis(draws)
      )
    }) %>%
    bind_rows() %>% 
    mutate(knap_id = 1:n())
}
form_knapsacks <- cmpfun(form_knapsacks)

# Calculate how often each knapsack will win when faced with n other knapsacks.
# Typically, n is the number of players
estimate_knapsack_utility <- function(knapsacks, n) {
  knapsacks <- knapsacks %>% 
    select(knap_id, knap_draws) %>% 
    unnest() %>% 
    group_by(knap_id) %>% 
    mutate(round = 1:n()) %>% 
    ungroup() %>% 
    group_by(round)
  
  comboGeneral(unique(knapsacks$knap_id), m = n) %>% 
    apply(1, function(matchup) {
      knapsacks %>%
        filter(knap_id %in% matchup) %>%
        summarise(winner = knap_id[max(knap_draws) == knap_draws][1],
                  matchup = paste0(matchup, collapse = ""))
    }) %>% 
    bind_rows() %>%
    group_by(matchup, winner) %>%
    summarise(n = n()) %>%
    mutate(win_prop = n/sum(n),
           # win all for winning, lose your entry fee for losing
           utility = ((n()-1)*win_prop - 1*(1-win_prop))) %>% 
    rename(knap_id = winner) %>% 
    select(-n)
}
estimate_knapsack_utility <- cmpfun(estimate_knapsack_utility)

# Strategy functions -------------------------------------------------------------------------------
# Given the knapsack (with estimated utilities), what knapsack would each function pick.

# Choose a random strategy
strat_random <- function(knaps) {
  selection <- sample(knaps$knap_id, 1)
  knaps %>% 
    filter(knap_id == selection)
}
