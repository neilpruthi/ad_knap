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
# For linear programming
require(dplyr)
require(ROI)
require(ROI.plugin.glpk)
require(ompr)
require(ompr.roi)
data(starwars)

# Functions ----------------------------------------------------------------------------------------

# Helper to get number of unique characters in string
nchar_unique <- function(s) {
  length(unique(strsplit(s , "", fixed = TRUE)[[1]]))
}
nchar_unique <- cmpfun(nchar_unique)

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
  
  permuteGeneral(sort(unique(knapsacks$knap_id)), n, repetition = T) %>% 
    apply(1, sort) %>% 
    t %>% 
    unique() %>% 
    apply(1, function(matchup) {
      knapsacks %>%
        filter(knap_id %in% matchup) %>%
        do({
          data_frame(knap_id = .$knap_id,
                     winner = .$knap_id[which(.$knap_draws == max(.$knap_draws))],
                     matchup = paste0(matchup, collapse = "")) %>% 
            mutate(
              # if there is a tie, no one wins!
              n_wins = ifelse(length(unique(.$winner)) > 1, 0, 1),
              # Both choose the same strategy, still a tie!
              n_wins = ifelse(nchar_unique(.$matchup) == 1, 0, n_wins),
              # make sure only the true winner gets credit for winning...
              n_wins = ifelse(winner == knap_id, n_wins, 0)
            )
        }) %>% 
        select(-winner) %>% 
        group_by(knap_id, matchup) %>% 
        summarise(n_wins = sum(n_wins))
    }) %>% 
    bind_rows() %>%
    group_by(matchup) %>%
    mutate(win_prop = (n_wins/sum(n_wins)),
           # win all for winning, lose your entry fee for losing
           utility = ((n()-1)*win_prop - 1*(1-win_prop))) %>% 
    select(-n_wins) %>% 
    replace_na(list(win_prop = 0, utility = 0))
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
strat_random <- cmpfun(strat_random)

# Two player minimax
strat_minimax_2 <- function(knaps) {
  
  row_strats <- sort(unique(knaps$knap_id))
  col_strats <- sort(unique(knaps$knap_id))
  
  MIPModel() %>%
    add_variable(row_utility, type = "continuous") %>%
    add_variable(row_pr[i], i = row_strats, type = "continuous", lb = 0, ub = 1) %>%
    set_objective(row_utility, "max") %>%
    add_constraint(sum_expr(row_pr[i]*knaps$utility[knaps$knap_id == i & knaps$matchup == paste0(sort(c(i, j)), collapse = "")], i = row_strats) >= row_utility,
                   j = col_strats) %>%
    add_constraint(sum_expr(row_pr[i], i = row_strats) == 1) %>%
    solve_model(with_ROI(solver = "glpk"))
}
strat_minimax_2 <- cmpfun(strat_minimax_2)

# Three player minimax
strat_minimax_3 <- function(knaps) {
  
  row_strats <- sort(unique(knaps$knap_id))
  col_strats <- sort(unique(knaps$knap_id))
  
  MIPModel() %>%
    add_variable(row_utility, type = "continuous") %>%
    add_variable(row_pr[i], i = row_strats, type = "continuous", lb = 0, ub = 1) %>%
    set_objective(row_utility, "max") %>%
    add_constraint(sum_expr(row_pr[i]*knaps$utility[knaps$knap_id == i & knaps$matchup == paste0(sort(c(i, j)), collapse = "")], i = row_strats) >= row_utility,
                   j = col_strats) %>%
    add_constraint(sum_expr(row_pr[i], i = row_strats) == 1) %>%
    solve_model(with_ROI(solver = "glpk"))
}
strat_minimax_3 <- cmpfun(strat_minimax_3)
# strat_minimax_3(tmp)
