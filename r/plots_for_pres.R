# Description --------------------------------------------------------------------------------------
# Generate plots for presentation.
# Ben Ewing on 2019-11-28

# Setup --------------------------------------------------------------------------------------------
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
library(ggplot2)
library(ggthemes)
theme_set(theme_tufte(base_size = 22))

source("r/knapsack_functions_mat.R")

n_draws <- 100000

# Two Item Two Player, Equal mean Equal Var --------------------------------------------------------
n_players <- 2
knap_size <- 1

tmp_mat <- matrix(c(
  rnorm(n_draws, 0, 1),
  rnorm(n_draws, 0, 1)
), ncol = 2)
tmp_u <- estimate_knapsack_utility(tmp_mat, n_players)

tmp_mat %>% 
  as_data_frame() %>% 
  gather(var, val) %>% 
  transmute(est_utility = ifelse(gsub("V", "", var) == 1, 
                                 tmp_u$utility[tmp_u$matchup == "1;2" & tmp_u$knap_id == 1],
                                 tmp_u$utility[tmp_u$matchup == "1;2" & tmp_u$knap_id == 2]),
            val = val, est_utility = round(est_utility, digits = 4),
            est_utility = factor(paste0("Estimated Utility: ", est_utility))) %>% 
  ggplot(aes(val, fill = est_utility, colour = est_utility)) +
  geom_density(alpha = 0.3) +
  facet_wrap(. ~ est_utility, ncol = 1) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = "none")
ggsave("figures/for_pres/two_player_two_item_equal_mu_sd.png", width = 16, height = 10)

# Two Item Two Player, Equal mean het Var --------------------------------------------------------
n_players <- 2
knap_size <- 1

tmp_mat <- matrix(c(
  rnorm(n_draws, 0, 1),
  rnorm(n_draws, 0, 5)
), ncol = 2)
tmp_u <- estimate_knapsack_utility(tmp_mat, n_players)

tmp_mat %>% 
  as_data_frame() %>% 
  gather(var, val) %>% 
  transmute(est_utility = ifelse(gsub("V", "", var) == 1, 
                                 tmp_u$utility[tmp_u$matchup == "1;2" & tmp_u$knap_id == 1],
                                 tmp_u$utility[tmp_u$matchup == "1;2" & tmp_u$knap_id == 2]),
            val = val, est_utility = round(est_utility, digits = 4)) %>% 
  mutate(est_utility = as.factor(paste0("Estimated Utility: ", est_utility))) %>% 
  ggplot(aes(val, fill = est_utility, colour = est_utility)) +
  geom_density(alpha = 0.3) +
  facet_wrap(. ~ est_utility, ncol = 1) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = "none")
ggsave("figures/for_pres/two_player_two_item_eq_mu_het_sd.png", width = 16, height = 10)

# three player 5 item Equal mean het Var --------------------------------------------------------
n_players <- 3
knap_size <- 1

tmp_mat <- matrix(c(
  rnorm(n_draws, 0, 1),
  rnorm(n_draws, 0, 2),
  rnorm(n_draws, 0, 3),
  rnorm(n_draws, 0, 4),
  rnorm(n_draws, 0, 5)
), ncol = 5)
tmp_u <- estimate_knapsack_utility(tmp_mat, n_players)

tmp_mat %>% 
  as_data_frame() %>% 
  gather(var, val) %>% 
  transmute(est_utility = case_when(
    gsub("V", "", var) == 1 ~ mean(tmp_u$utility[tmp_u$knap_id == 1]),
    gsub("V", "", var) == 2 ~ mean(tmp_u$utility[tmp_u$knap_id == 2]),
    gsub("V", "", var) == 3 ~ mean(tmp_u$utility[tmp_u$knap_id == 3]),
    gsub("V", "", var) == 4 ~ mean(tmp_u$utility[tmp_u$knap_id == 4]),
    gsub("V", "", var) == 5 ~ mean(tmp_u$utility[tmp_u$knap_id == 5])
  ), val = val, est_utility = round(est_utility, digits = 4)) %>% 
  mutate(est_utility = factor(est_utility, levels = sort(est_utility),
                                 labels = paste0("Average Estimated Utility: ", sort(est_utility)))) %>% 
  ggplot(aes(val, fill = est_utility, colour = est_utility)) +
  geom_density(alpha = 0.3) +
  facet_wrap(. ~ est_utility, ncol = 1) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = "none")
ggsave("figures/for_pres/three_player_five_item_het_mu_eq_sd.png", width = 16, height = 10)

# three player 5 item het mean het Var --------------------------------------------------------
n_players <- 3
knap_size <- 1

tmp_mat <- matrix(c(
  rnorm(n_draws, 5, 1),
  rnorm(n_draws, 4, 2),
  rnorm(n_draws, 3, 3),
  rnorm(n_draws, 2, 4),
  rnorm(n_draws, 1, 5) 
), ncol = 5)
tmp_u <- estimate_knapsack_utility(tmp_mat, n_players)
tmp_u_mini <- strat_minimax_3(tmp_u, n_players)

tmp_mat %>% 
  as_data_frame() %>% 
  gather(var, val) %>% 
  transmute(est_utility = case_when(
    gsub("V", "", var) == 1 ~ mean(tmp_u$utility[tmp_u$knap_id == 1]),
    gsub("V", "", var) == 2 ~ mean(tmp_u$utility[tmp_u$knap_id == 2]),
    gsub("V", "", var) == 3 ~ mean(tmp_u$utility[tmp_u$knap_id == 3]),
    gsub("V", "", var) == 4 ~ mean(tmp_u$utility[tmp_u$knap_id == 4]),
    gsub("V", "", var) == 5 ~ mean(tmp_u$utility[tmp_u$knap_id == 5])
  ),
  minimax_strat = case_when(
    gsub("V", "", var) == 1 ~ mean(tmp_u_mini$probability[tmp_u_mini$knap_id == 1]),
    gsub("V", "", var) == 2 ~ mean(tmp_u_mini$probability[tmp_u_mini$knap_id == 2]),
    gsub("V", "", var) == 3 ~ mean(tmp_u_mini$probability[tmp_u_mini$knap_id == 3]),
    gsub("V", "", var) == 4 ~ mean(tmp_u_mini$probability[tmp_u_mini$knap_id == 4]),
    gsub("V", "", var) == 5 ~ mean(tmp_u_mini$probability[tmp_u_mini$knap_id == 5])
  ) %>% round(., digits = 4), 
  val = val, est_utility = round(est_utility, digits = 4)) %>% 
  mutate(est_utility = factor(est_utility, levels = sort(est_utility),
                                 labels = paste0("Average Estimated Utility: ", sort(est_utility)))) %>% 
  ggplot(aes(val, fill = minimax_strat, colour = minimax_strat)) +
  geom_density(alpha = 0.3) +
  facet_wrap(. ~ est_utility, ncol = 1) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = "none")
ggsave("figures/for_pres/three_player_five_item_het_mu_het_sd.png", width = 16, height = 10)


# Three player minimax -----------------------------------------------------------------------------
# Load the single item knapsacks
load("sims/single_item_knapsacks_2018_11_27.Rdata")
# Look only at three player instances, and find minimax strategies
tmp <- map_df(1:(length(tmp)-1), ~ {
  if(tmp[[.x]]$n_players == 3)
    strat_minimax_3(tmp[[.x]])
})

tmp %>% 
  group_by(uid, knap_id) %>% 
  summarise_if(is.numeric, mean) %>% 
  ungroup() %>% 
  select(probability, utility, mean, sd, skewness, kurtosis) %>% 
  gather(var, val, -probability) %>% 
  ggplot(aes(x = val, y = probability)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  facet_wrap( ~ var, scales = "free", ncol = 2)
