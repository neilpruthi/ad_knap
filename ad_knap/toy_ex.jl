# Programs for choosing objects maximizing expected value.
# Ben Ewing on 2018-10-30

# Setup ------------------------------------------------------------------------
# Libraries
using JuMP
using Distributions
using LinearAlgebra
using GLPKMathProgInterface

# Settings
# Arbitrary budget
budget = 10
# Number of items to consider
n_items = 300
# Max items to buy
max_items = 10
# Number of item types
n_item_types = 3
# Solver
m = Model(solver = GLPKSolverMIP())

# For an item of a single time -------------------------------------------------
# Mean and sd for each object from a uniform distribution
mean = rand(Uniform(0, 1), n_items)
sd = rand(Uniform(0, 1), n_items)
# Price is a function of mean and sd
price = map((μ, σ) -> 20*abs(rand(Normal(μ, σ))), mean, sd)
# Actual value is a draw from the distribution of each object
real_value = map((μ, σ) -> rand(Normal(μ, σ)), mean, sd)

# To buy or not to buy?
@variable(m, buy[1:n_items], Bin)
# Max expected mean
@objective(m, Max, dot(mean, buy))
# s.t. budget binds
@constraint(m, dot(price, buy) <= budget)
# s.t. can buy at most ten items
@constraint(m, sum(buy) <= max_items)

status = solve(m)
opt_buy = map(x -> convert(Bool, x), getvalue(buy))

println("Number of items: ", sum(opt_buy))
println("Budget used: ", sum(price[opt_buy]))
println("Predicted value: ", getobjectivevalue(m))
println("Achieved value: ", sum(real_value[opt_buy]))
println("---")

# Maximizing variance instead of expectation -----------------------------------

# Items of multiple types ------------------------------------------------------
# This is actually a pretty straightforward extension

# Just to make the types a little different, choose a top  value each can take

# Mean and sd for each object/type from a uniform distribution
mean = rand(Uniform(0, 1), n_items)
sd = rand(Uniform(0, 1), n_items)
# Price is a function of mean and sd
price = map((μ, σ) -> 20*abs(rand(Normal(μ, σ))), mean, sd)
# Actual value is a draw from the distribution of each object
real_value = map((μ, σ) -> rand(Normal(μ, σ)), mean, sd)

# To buy or not to buy?
@variable(m, buy[1:n_items], Bin)
# Max expected mean
@objective(m, Max, dot(mean, buy))
# s.t. budget binds
@constraint(m, dot(price, buy) <= budget)
# s.t. can buy at most ten items
@constraint(m, sum(buy) <= max_items)

status = solve(m)
opt_buy = map(x -> convert(Bool, x), getvalue(buy))

println("Number of items: ", sum(opt_buy))
println("Budget used: ", sum(price[opt_buy]))
println("Predicted value: ", getobjectivevalue(m))
println("Achieved value: ", sum(real_value[opt_buy]))
println("---")

# Items with correlated distributions ------------------------------------------
