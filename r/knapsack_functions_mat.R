# Description --------------------------------------------------------------------------------------
# Functions to simulate adversarial knapsack game. This version uses matrices where possible. This
# gives us a huge increase in speed.
# Ben Ewing on 2018-11-26

# Setup --------------------------------------------------------------------------------------------
# Libraries
# For kurtosis and skew functions
require(e1071)
require(dplyr)
# RcppAlgos::comboGeneral is ~80x faster than utils::combn in my quick test, it also seems to beat
# data.table::CJ. It can also run in parallel, but this doesn't seem to help on Windows. Might be
# worth trying on the Econ linux machines?
require(RcppAlgos)
# For compiling functions to bytecode, this usually nets a tiny speedup
require(compiler)

# Testing ------------------------------------------------------------------------------------------
# Number of draws from each item's dist
n_draws <- 1000

# Each col is an item, each row is a draw from the dist
items <- matrix(c(
  rnorm(n_draws, 0, 0),
  rnorm(n_draws, 1, 0),
  rnorm(n_draws, 2, 0),
  rnorm(n_draws, 5, 2),
  rnorm(n_draws, 4, 1),
  rnorm(n_draws, 6, 1.257)
), ncol = 6)

# Functions ----------------------------------------------------------------------------------------
form_knapsacks <- function(items, size) {
  comboGeneral(1:ncol(items), m = size) %>% 
    apply(1, function(x) {
      if (length(x) == 1) 
        items[, x]
      else 
        rowSums(items[, x])
    })
}
form_knapsacks <- cmpfun(form_knapsacks)
knapsacks <- form_knapsacks(items, 2)

estimate_knapsack_utility <- function(knapsacks, n_players) {
  nck <- ncol(knapsacks)
  nrk <- nrow(knapsacks)
  # For each possible knapsack selection by each player
  permuteGeneral(1:ncol(knapsacks), n_players, repetition = T) %>% 
    apply(1, sort) %>% 
    t %>% 
    unique() %>% 
    split(., seq(nrow(.))) %>% 
    map_dfr(function(matchup) {
      if (length(unique(matchup)) == 1) {
        kmean = mean(knapsacks[ , unique(matchup)])
        ksd = sd(knapsacks[ , unique(matchup)])
        kskewness = skewness(knapsacks[ , unique(matchup)])
        kkurtosis = kurtosis(knapsacks[ , unique(matchup)])
        
        data_frame(
          knap_id = rep(unique(matchup), length(matchup)),
          matchup = paste0(matchup, collapse = ";"),
          win_prop = 0, utility = 0,
          mean = kmean, 
          sd = ksd, 
          skewness = kskewness,
          kurtosis = kkurtosis
        )
      } else {
        # for each knapsack in the matchup
        # How often does this knapsack win against the others?
        nck_match <- ncol(knapsacks[ , matchup])
        map_dfr(1:nck_match, function(current_knap) {
          win_prop = (sum((knapsacks[ , matchup][ , current_knap] > knapsacks[ , matchup][ , -current_knap]) == T)/(nrk*(n_players-1)))
          kmean = mean(knapsacks[ , unique(matchup)])
          ksd = sd(knapsacks[ , unique(matchup)])
          kskewness = skewness(knapsacks[ , unique(matchup)])
          kkurtosis = kurtosis(knapsacks[ , unique(matchup)])
          
          data_frame(
            knap_id = matchup[current_knap],
            matchup = paste0(matchup, collapse = ";"),
            win_prop = win_prop,
            utility = ((n_players-1)*win_prop - 1*(1-win_prop)),
            mean = kmean, 
            sd = ksd, 
            skewness = kskewness,
            kurtosis = kkurtosis
          )
        })
      }
    })
}

estimate_knapsack_utility <- cmpfun(estimate_knapsack_utility)
knapsacks_u2 <- estimate_knapsack_utility(knapsacks, 2)
knapsacks_u3 <- estimate_knapsack_utility(knapsacks, 3)

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
    add_constraint(sum_expr(row_pr[i]*knaps$utility[knaps$knap_id == i & knaps$matchup == paste0(sort(c(i, j)), collapse = ";")], i = row_strats) >= row_utility,
                   j = col_strats) %>%
    add_constraint(sum_expr(row_pr[i], i = row_strats) == 1) %>%
    solve_model(with_ROI(solver = "glpk"))
}
strat_minimax_2 <- cmpfun(strat_minimax_2)

# Three player minimax
strat_minimax_3 <- function(knaps) {
  
  strats <- sort(unique(knaps$knap_id))
  
  MIPModel() %>%
    add_variable(row_utility, type = "continuous") %>%
    add_variable(row_pr[i], i = strats, type = "continuous", lb = 0, ub = 1) %>%
    set_objective(row_utility, "max") %>%
    add_constraint(sum_expr(row_pr[i]*knaps$utility[knaps$knap_id == i & knaps$matchup == paste0(sort(c(i, j, k)), collapse = ";")], i = strats) >= row_utility,
                   j = strats, k = strats) %>%
    add_constraint(sum_expr(row_pr[i], i = strats) == 1) %>%
    solve_model(with_ROI(solver = "glpk"))
}
strat_minimax_3 <- cmpfun(strat_minimax_3)
strat_minimax_3(knapsacks_u3)
