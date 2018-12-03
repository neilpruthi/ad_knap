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
      # for each knapsack in the matchup
      # How often does this knapsack win against the others?
      map_dfr(1:length(matchup), function(current_match) {
        # Borrowing Mory's code
        # Sometimes subtracting columns leaves a vector, which apply will not work with
        
        current_knap <- knapsacks[ , matchup[current_match]]
        opp_knaps <- knapsacks[ , matchup[-current_match]]
        
        if (class(opp_knaps) == "matrix")
          tie_cols <- colSums(current_knap == opp_knaps) == nrow(knapsacks)
        else
          tie_cols <- sum(opp_knaps == current_knap) == nrow(knapsacks)
        
        # subset out any tying columns
        if (any(tie_cols) & class(opp_knaps) == "matrix")
          opp_knaps <- opp_knaps[ , !tie_cols]
        else if (any(tie_cols))
          opp_knaps <- opp_knaps[!tie_cols]
        
        # print("current_knap")
        # print(current_knap)
        # print("opp_knaps")
        # print(opp_knaps)
        
        # if empty, then all were ties!
        if (length(opp_knaps) == 0)
          maxelse <- current_knap
        # otherwise, get maxelse
        else if(class(opp_knaps) == "matrix")
          maxelse <- apply(opp_knaps, 1, max)
        else
          maxelse <- opp_knaps
        
        # print("maxelse")
        # print(maxelse)
        
        win_n <- sum(current_knap > maxelse)
        
        win_prop <- win_n/n_rounds
        
        # print("win_n")
        # print(win_n)
        # print("win_prop")
        # print(win_prop)
        
        kmean <- mean(current_knap)
        ksd <- sd(current_knap)
        kskewness <- skewness(current_knap)
        kkurtosis <- kurtosis(current_knap)
        
        data_frame(
          knap_id = matchup[current_match],
          matchup = paste0(matchup, collapse = ";"),
          n_tie_strats = sum(tie_cols),
          win_n = win_n,
          win_prop = win_prop,
          mean = kmean, 
          sd = ksd, 
          skewness = kskewness,
          kurtosis = kkurtosis
        )
      })
    }) %>% 
    # determine utility in each matchup
    group_by(matchup) %>% 
    mutate(
      utility = case_when(
        sum(win_prop) == 0 ~ 0,
        # tie win
        win_prop == max(win_prop) ~ win_prop * (n()/(sum(win_prop == max(win_prop))) - 1) - 1*(1-win_prop),
        # tie lose
        win_prop == min(win_prop) ~ win_prop * (n()/(sum(win_prop == min(win_prop))) - 1) - 1*(1-win_prop),
        # otherwise
        T ~ (n_players-1)*win_prop - 1*(1-win_prop)
      )
    )
}
estimate_knapsack_utility <- cmpfun(estimate_knapsack_utility)

# Strategy functions -------------------------------------------------------------------------------
# Given the knapsack (with estimated utilities), what knapsack would each function pick.

# Choose a random strategy
strat_random <- function(knaps, n = 1) {
  selection <- sample(knaps$knap_id, n)
  knaps %>% 
    mutate(probability = ifelse(knap_id == selection, 1/n, 0))
}
strat_random <- cmpfun(strat_random)

# Two player minimax
strat_minimax_2 <- function(knaps, print_results = F) {
  
  row_strats <- sort(unique(knaps$knap_id))
  col_strats <- sort(unique(knaps$knap_id))
  
  tmp <- suppressWarnings(
    MIPModel() %>%
      add_variable(row_utility, type = "continuous") %>%
      add_variable(row_pr[i], i = row_strats, type = "continuous", lb = 0, ub = 1) %>%
      set_objective(row_utility, "max") %>%
      add_constraint(sum_expr(row_pr[i]*knaps$utility[knaps$knap_id == i & knaps$matchup == paste0(sort(c(i, j)), collapse = ";")], i = row_strats) >= row_utility,
                     j = col_strats) %>%
      add_constraint(sum_expr(row_pr[i], i = row_strats) == 1) %>%
      solve_model(with_ROI(solver = "glpk"))
  )
  if (print_results)
    print(tmp$solution)
  data_frame(knap_id = strats, 
             probability = as.numeric(tmp$solution)[-(length(strats)+1)]) %>% 
    left_join(knaps, by = "knap_id")
}
strat_minimax_2 <- cmpfun(strat_minimax_2)

# Three player minimax
strat_minimax_3 <- function(knaps, print_results = F) {
  
  strats <- sort(unique(knaps$knap_id))
  
  tmp <- suppressWarnings(
    MIPModel() %>%
      add_variable(row_utility, type = "continuous") %>%
      add_variable(row_pr[i], i = strats, type = "continuous", lb = 0, ub = 1) %>%
      set_objective(row_utility, "max") %>%
      add_constraint(sum_expr(row_pr[i]*knaps$utility[knaps$knap_id == i & knaps$matchup == paste0(sort(c(i, j, k)), collapse = ";")], i = strats) >= row_utility,
                     j = strats, k = strats) %>%
      add_constraint(sum_expr(row_pr[i], i = strats) == 1) %>%
      solve_model(with_ROI(solver = "glpk"))
  )
  if (print_results)
    print(tmp$solution)
  data_frame(knap_id = strats, 
             probability = as.numeric(tmp$solution)[-(length(strats)+1)]) %>% 
    left_join(knaps, by = "knap_id")
}
strat_minimax_3 <- cmpfun(strat_minimax_3)
