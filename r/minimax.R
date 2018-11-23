#' Given a matrix of utilities for two players returns the minimax solution for the row player.
#' 
#' Matching Pennies:
#' minimax_2(matrix(c(1, -1, -1, 1), nrow = 2))$solution
minimax_2 <- function(utilities) {
  require(dplyr)
  require(ROI)
  require(ROI.plugin.glpk)
  require(ompr)
  require(ompr.roi)
  
  row_strats <- 1:dim(utilities)[1]
  col_strats <- 1:dim(utilities)[2]
  
  MIPModel() %>%
    add_variable(row_utility, type = "continuous") %>%
    add_variable(row_pr[i], i = row_strats, type = "continuous", lb = 0, ub = 1) %>%
    set_objective(row_utility, "max") %>%
    add_constraint(sum_expr(row_pr[i]*utilities[i, j], i = row_strats) >= row_utility,
                   j = col_strats) %>%
    add_constraint(sum_expr(row_pr[i], i = row_strats) == 1) %>%
    solve_model(with_ROI(solver = "glpk"))
}

#' Given a matrix of utilities for three players returns the minimax solution for the first player.
#' !!Probably not working!!
minimax_3 <- function(utilities) {
  require(ROI)
  require(ROI.plugin.glpk)
  require(ompr)
  require(ompr.roi)
  
  x_strats <- 1:dim(utilities)[1]
  y_strats <- 1:dim(utilities)[2]
  z_strats <- 1:dim(utilities)[3]
  
  MIPModel() %>%
    add_variable(x_u, type = "continuous") %>%
    add_variable(x_pr[i], i = x_strats, type = "continuous", lb = 0, ub = 1) %>%
    set_objective(x_u, "max") %>%
    add_constraint(sum_expr(x_pr[i]*utilities[i, j, k], i = x_strats) >= x_u,
                   j = y_strats, k = z_strats) %>%
    add_constraint(sum_expr(x_pr[i], i = x_strats) == 1) %>%
    solve_model(with_ROI(solver = "glpk"))
}
