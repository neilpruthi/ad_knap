minimax <- function(utilities, n_players) {
  # Player strats
  strats <- unique(map_chr(names(utilities), ~ substr(.x, 1, 1)))
  # Indices
  strats_i <- 1:length(strats)
  m <- MIPModel() %>%
    add_variable(utility, type = "continuous") %>%
    add_variable(pr[i], i = strats_i, type = "continuous", lb = 0, ub = 1) %>%
    set_objective(utility, "max") %>%
    add_constraint(sum_expr(pr[i], i = strats_i) == 1) %>% 
    add_constraint(sum_expr(pr[i]*utilities[[paste0(strats[i], strats[j])]], 
                            i = strats_i) >= utility, 
                   j = strats_i)
  solve_model(m, with_ROI(solver = "glpk"))
}