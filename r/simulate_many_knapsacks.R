# Description --------------------------------------------------------------------------------------
# Simualte many knapsacks and save results.
# Ben Ewing on 2018-11-26

# Setup --------------------------------------------------------------------------------------------
# For parallization
library(future)
library(furrr)
plan(multiprocess)
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
# For linear programming
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)

source("r/knapsack_functions_mat.R")

date <- gsub("-", "_", Sys.Date())

# Mean-variance tradeoff ---------------------------------------------------------------------------
sim_settings <- expand.grid(
  n_draws = 1000,
  n_players = 3,
  knap_size = 1,
  n_items = 3,
  mean = 5:10,
  sd = seq(1, 5, 0.5)
) %>% mutate(uid = 1:n())

tmp <- future_pmap(sim_settings, function(n_draws, n_players, knap_size, n_items, mean, sd, uid) {
  matrix(c(
    rnorm(n_draws, 5, 1),
    rnorm(n_draws, 5, 3),
    rnorm(n_draws, mean, sd)
  ), ncol = 3) %>% 
    form_knapsacks(knap_size) %>% 
    estimate_knapsack_utility(n_players) %>% 
    mutate(n_draws = n_draws, n_players = n_players, knap_size = knap_size, 
           n_items = n_items, uid = uid)
})
tmp$settings <- "n_draws = 1000; n_players = 3; knap_size = 1; n_items = 3; mean = 5:10; sd = seq(1, 5, 0.5)"
save(tmp, file = paste0("sims/mean_var_tradeoff_", date, ".Rdata"))

map(1:(length(tmp)-1), ~ strat_minimax_3(tmp[[.x]])$solution)

# Effect of number of players and knapsack size ----------------------------------------------------
# sim_settings <- expand.grid(
#   n_draws = 1000,
#   n_players = 2:10,
#   knap_size = 1:10,
#   n_items = 10
# ) %>% mutate(uid = 1:n())
# 
# tmp <- future_pmap(sim_settings, function(n_draws, n_players, knap_size, n_items, uid) {
#   do.call(cbind, lapply(1:n_items, function(x) rnorm(n_draws, runif(1, 0, 10), runif(1, 0, 10)))) %>% 
#     form_knapsacks(knap_size) %>% 
#     estimate_knapsack_utility(n_players) %>% 
#     mutate(n_draws = n_draws, n_players = n_players, knap_size = knap_size, 
#            n_items = n_items, uid = uid)
# })
# tmp$settings <- "n_draws = 1000; n_players = 1:10; knap_size = 1:10; n_items = 10"
# save(tmp, file = paste0("sims/effects_of_n_player_knap_size_", date,".Rdata"))
