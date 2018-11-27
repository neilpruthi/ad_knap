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

# Functions ----------------------------------------------------------------------------------------

# Helper to get number of unique characters in string
nchar_unique <- function(s) {
  length(unique(strsplit(s , "", fixed = TRUE)[[1]]))
}
nchar_unique <- cmpfun(nchar_unique)

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

# For possibly large knapsacks, just take a sample
form_knapsack_sample <- function(items, size, n) {
  comboSample(1:ncol(items), m = size, n = n) %>% 
    apply(1, function(x) {
      if (length(x) == 1) 
        items[, x]
      else 
        rowSums(items[, x])
    })
}
form_knapsack_sample <- cmpfun(form_knapsack_sample)

estimate_knapsack_utility <- function(knapsacks, n_players) {
  n_knaps <- ncol(knapsacks)
  n_rounds <- nrow(knapsacks)
  # For each possible knapsack selection by each player
  permuteGeneral(1:ncol(knapsacks), n_players, repetition = T) %>% 
    apply(1, sort) %>% 
    t %>% 
    unique() %>% 
    split(., seq(nrow(.))) %>% 
    map_dfr(function(matchup) {
      if (length(unique(matchup)) == 1) {
        kmean <- mean(knapsacks[ , unique(matchup)])
        ksd <- sd(knapsacks[ , unique(matchup)])
        kskewness <- skewness(knapsacks[ , unique(matchup)])
        kkurtosis <- kurtosis(knapsacks[ , unique(matchup)])
        
        data_frame(
          knap_id = unique(matchup),
          matchup = paste0(matchup, collapse = ";"),
          n_rounds = n_rounds,
          n_ties = n_rounds, 
          n_tie_strats = 1, 
          n_wins = 0, 
          utility = 0,
          mean = kmean, 
          sd = ksd, 
          skewness = kskewness,
          kurtosis = kkurtosis
        )
      } else {
        # for each knapsack in the matchup
        # How often does this knapsack win against the others?
        n_knaps_match <- ncol(knapsacks[ , matchup])
        map_dfr(1:n_knaps_match, function(current_knap) {
          # Borrowing Mory's code
          # Sometimes subtracting columns leaves a vector, which apply will not work with
          if(class(knapsacks[ , matchup][ , -current_knap]) == "matrix") {
            maxelse <- apply(knapsacks[ , matchup][ , -current_knap], 1, max)
            tie_cols <- apply(knapsacks[ , matchup][ , -current_knap], 2, function(x) all(x == knapsacks[ , matchup][ , current_knap]))
          }
          else {
            maxelse <- knapsacks[ , matchup][ , -current_knap]
            tie_cols <- rep(F, length(maxelse))
          }
          
          win_n <- sum(knapsacks[ , matchup][ , current_knap] > maxelse)
          tie_n <- sum(knapsacks[ , matchup][ , current_knap] == maxelse)
          win_prop <- win_n/(length(maxelse))#*(n_players-1))
          
          # If tie_n == nrow, and win_prop > 1/n_players then just divide utility among tied winners
          if (tie_n == length(maxelse)) {
            utility <- sum(!tie_cols)/(sum(tie_cols)+1)
          }
          else
            utility <- (((n_players-1)*win_prop) - 1*(1-win_prop))
          
          kmean <- mean(knapsacks[ , matchup][ , current_knap])
          ksd <- sd(knapsacks[ , matchup][ , current_knap])
          kskewness <- skewness(knapsacks[ , matchup][ , current_knap])
          kkurtosis <- kurtosis(knapsacks[ , matchup][ , current_knap])
          
          # if multiple players play the same winning strategy, they need to split the payout,
          # but if they lose then they lose all
          
          data_frame(
            knap_id = matchup[current_knap],
            matchup = paste0(matchup, collapse = ";"),
            n_rounds = n_rounds,
            n_ties = tie_n,
            n_tie_strats = sum(tie_cols),
            n_wins = win_n,
            # win_prop = win_prop, 
            utility = utility,
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
  
  suppressWarnings(
    MIPModel() %>%
      add_variable(row_utility, type = "continuous") %>%
      add_variable(row_pr[i], i = row_strats, type = "continuous", lb = 0, ub = 1) %>%
      set_objective(row_utility, "max") %>%
      add_constraint(sum_expr(row_pr[i]*knaps$utility[knaps$knap_id == i & knaps$matchup == paste0(sort(c(i, j)), collapse = ";")], i = row_strats) >= row_utility,
                     j = col_strats) %>%
      add_constraint(sum_expr(row_pr[i], i = row_strats) == 1) %>%
      solve_model(with_ROI(solver = "glpk"))
  )
}
strat_minimax_2 <- cmpfun(strat_minimax_2)

# Three player minimax
strat_minimax_3 <- function(knaps) {
  
  strats <- sort(unique(knaps$knap_id))
  
  suppressWarnings(
    MIPModel() %>%
      add_variable(row_utility, type = "continuous") %>%
      add_variable(row_pr[i], i = strats, type = "continuous", lb = 0, ub = 1) %>%
      set_objective(row_utility, "max") %>%
      add_constraint(sum_expr(row_pr[i]*knaps$utility[knaps$knap_id == i & knaps$matchup == paste0(sort(c(i, j, k)), collapse = ";")], i = strats) >= row_utility,
                     j = strats, k = strats) %>%
      add_constraint(sum_expr(row_pr[i], i = strats) == 1) %>%
      solve_model(with_ROI(solver = "glpk"))
  )
}
strat_minimax_3 <- cmpfun(strat_minimax_3)
