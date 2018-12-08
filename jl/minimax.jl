# Minimax function for n players. Assumes players can all play the same strats.
# Ben Ewing on 2018-12-03

# Setup ------------------------------------------------------------------------
# Libraries
using JuMP, Distributions, LinearAlgebra, GLPKMathProgInterface, Combinatorics

# Two player minimax -----------------------------------------------------------
function minimax_2(knaps)
    # Solver
    m = Model(solver = GLPKSolverMIP())
    # number of strategies
    nstrat = size(knaps)[1]
    # player utility
    @variable(m, utility)
    # probability of playing a given knapsack
    @variable(m, knap_pr[1:nstrat] >= 0)
    # Objective is to maximize utility
    @objective(m, Max, utility)
    # s.t. probability sums to 1
    @constraint(m, sum(knap_pr) == 1)
    # s.t. utility determined by knapsacks
    for j in 1:nstrat
        @constraint(m, sum{knap_pr[i]*knaps[i, j], i = 1:nstrat} >= utility)
    end
    status = solve(m)
    getvalue(knap_pr)
end

# Three player minimax ---------------------------------------------------------
function minimax_3(knaps)
    # Solver
    m = Model(solver = GLPKSolverMIP())
    # number of strategies, assume square in all directions
    nstrat = size(knaps)[1]
    # player utility
    @variable(m, utility)
    # probability of playing a given knapsack
    @variable(m, knap_pr[1:nstrat] >= 0)
    # Objective is to maximize utility
    @objective(m, Max, utility)
    # s.t. probability sums to 1
    @constraint(m, sum(knap_pr) == 1)
    # s.t. utility determined by knapsacks
    for k in 1:nstrat
        for j in 1:nstrat
            @constraint(m, sum{knap_pr[i]*knaps[i, j, k], i = 1:nstrat} >= utility)
        end
    end
    status = solve(m)
    getvalue(knap_pr)
end

# n player minimax -------------------------------------------------------------
function minimax(knaps)
    # Solver
    m = Model(solver = GLPKSolverMIP())
    # number of strategies
    nstrat = size(knaps)[1]
    n_player = ndims(knaps)
    # player utility
    @variable(m, utility)
    # probability of playing a given knapsack
    @variable(m, knap_pr[1:nstrat] >= 0)
    # Objective is to maximize utility
    @objective(m, Max, utility)
    # s.t. probability sums to 1
    @constraint(m, sum(knap_pr) == 1)
    # s.t. utility determined by knapsacks

    for j in multiset_permutations(repeat(collect(1:nstrat), n_player), n_player-1)
        @constraint(m, sum{knap_pr[i]*knaps[i, j...], i = 1:nstrat} >= utility)
    end

    status = solve(m)
    getvalue(knap_pr)
end

# Testing ----------------------------------------------------------------------
# Two player
# Matching Pennies utilities
# minimax_2(hcat([1,-1], [-1,1]))
# minimax(hcat([1,-1], [-1,1]))
# rock paper scissors
# minimax_2(hcat([0, 1, -1], [-1, 0, 1], [1, -1, 0]))
# minimax(hcat([0, 1, -1], [-1, 0, 1], [1, -1, 0]))
# Three player
# 1-2-3-4
# minimax_3(cat([1 2; 3 4], [-1 -2; -3 -4], dims = 3))
# minimax(cat([1 2; 3 4], [-1 -2; -3 -4], dims = 3))
