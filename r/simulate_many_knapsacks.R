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

# Plotting
library(ggplot2)
library(ggthemes)
theme_set(theme_tufte())

source("r/knapsack_functions_mat.R")

date <- gsub("-", "_", Sys.Date())

# Single item knapsacks ----------------------------------------------------------------------------
sim_settings <- expand.grid(
  n_draws = 1000,
  n_players = 3,
  knap_size = 1,
  n_items = 10
) %>% mutate(uid = 1:n())

tmp <- pmap(sim_settings, function(n_draws, n_players, knap_size, n_items, mean, sd, uid) {
  mat <- do.call(cbind, lapply(1:n_items, function(x) rnorm(n_draws, runif(1, 0, 10), runif(1, 0, 10)))) %>% 
    estimate_knapsack_utility(n_players) %>%
    mutate(n_draws = n_draws, n_players = n_players, knap_size = knap_size,
           n_items = n_items, uid = uid)
})
tmp$settings <- sim_settings
save(tmp, file = paste0("sims/single_item_knapsacks_", date, ".Rdata"))

# Look at three player instances
tmp <- map_df(1:(length(tmp)-1), ~ {
  if(tmp[[.x]]$n_players == 3)
    strat_minimax_3(tmp[[.x]])
})

# plot
tmp %>% 
  split(.$uid) %>% 
  walk(function(run) {
    n_items <- length(unique(run$knap_id))
    print(n_items)
    run <- run %>% 
      arrange(desc(probability)) %>% 
      filter(probability > 0) %>% 
      mutate(weighted_utility = probability*utility, knap_id = as.factor(knap_id)) %>% 
      select(knap_id, utility, weighted_utility) %>% 
      gather(measure, val, -knap_id)
    u_mean <- mean(run$val[run$measure == "utility"])
    wu_mean <- mean(run$val[run$measure == "weighted_utility"])
    ggplot(run, aes(val, fill = knap_id, colour = knap_id)) + 
      geom_density(alpha = 0.2) + 
      facet_wrap(. ~ measure, ncol = 1) +
      geom_vline(xintercept = u_mean, colour = "blue") +
      geom_vline(xintercept = wu_mean, colour = "red") +
      ggtitle(paste0("n_items = ", n_items)) +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    ggsave(paste0("figures/single_item_knaps/n_items_", n_items, ".png"),
           width = 16, height = 10)
  })

# Mean-variance tradeoff ---------------------------------------------------------------------------
# sim_settings <- expand.grid(
#   n_draws = 1000,
#   n_players = 3,
#   knap_size = 1,
#   n_items = 3,
#   mean = 5:10,
#   sd = seq(1, 5, 0.5)
# ) %>% mutate(uid = 1:n())
# 
# tmp <- future_pmap(sim_settings, function(n_draws, n_players, knap_size, n_items, mean, sd, uid) {
#   matrix(c(
#     rnorm(n_draws, 5, 1),
#     rnorm(n_draws, 5, 3),
#     rnorm(n_draws, mean, sd)
#   ), ncol = 3) %>% 
#     estimate_knapsack_utility(n_players) %>% 
#     mutate(n_draws = n_draws, n_players = n_players, knap_size = knap_size, 
#            n_items = n_items, uid = uid)
# })
# tmp$settings <- sim_settings
# save(tmp, file = paste0("sims/mean_var_tradeoff_", date, ".Rdata"))
# 
# map(1:(length(tmp)-1), ~ strat_minimax_3(tmp[[.x]]))

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
# tmp$settings <- sim_settings
# save(tmp, file = paste0("sims/effects_of_n_player_knap_size_", date,".Rdata"))
