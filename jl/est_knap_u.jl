function est_knap_u(item_draws, n_players)
    n_rounds = size(item_draws)[1]
    n_knaps = size(item_draws)[2]
    utilities = Dict()

    for match in Iterators.product([collect(1:n_knaps) for i in 1:n_players])
        match_s = string(match)
        utility = zeros(n_rounds)
        println("match_s: ", match_s)
        for round in 1:n_rounds
            print("round: ", round)
            opp_max = maximum(item_draws[round, match[1:end .!= match[1]])
            # actual_max = maximum(item_draws[:, match])

            print("opp_max: ", opp_max)
        end
    end
end

est_knap_u([1 2 3 4; 5 6 7 8; 9 10 11 12; 13 14 15 16], 2)
