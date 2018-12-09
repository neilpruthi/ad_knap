minimax <- function(utilities, n_players) {
  # Player strats
  strats <- unique(map_chr(names(utilities), ~ unlist(strsplit(.x, ";"))[1]))
  opp_strats <- unique(map_chr(names(utilities), ~ paste0(unlist(strsplit(.x, ";"))[2:n_players], 
                                                          collapse = ";")))
  # Indices
  strats_i <- 1:length(strats)
  opp_strats_i <- 1:length(opp_strats)
  
  m <- MIPModel() %>%
    add_variable(utility, type = "continuous") %>%
    add_variable(pr[i], i = strats_i, type = "continuous", lb = 0, ub = 1) %>%
    set_objective(utility, "max") %>%
    add_constraint(sum_expr(pr[i], i = strats_i) == 1) %>% 
    add_constraint(sum_expr(pr[i]*utilities[[paste0(strats[i], ";", opp_strats[j])]], 
                            i = strats_i) >= utility, 
                   j = opp_strats_i) %>% 
    solve_model(with_ROI(solver = "glpk"))
}
