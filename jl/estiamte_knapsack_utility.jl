# Function to estimate knapsack utility.
# Ben Ewing on 2018-12-03

using Combinatorics

function estimate_knapsack_utility(knaps, n_players)
    n_rounds = size(knaps)[1]
    n_knaps = size(knaps)[2]
    # count number of wins per knapsack
    out = zeros(Float64, [n_knaps for x = 1:n_players]...)

    # Get all unique matchups
    all_matchups = []
    for x in permutations(repeat(1:n_knaps, n_players), n_players)
        append!(all_matchups, [sort(x)])
    end
    all_matchups = unique(all_matchups)

    # Estimate win prop for each
    for matchup in all_matchups

        n_wins = zeros(length(matchup))
        n_ties = zeros(length(matchup))

        for knap in 1:length(matchup)
            player_knap = knaps[:, matchup[knap]...]
            opp_knaps = knaps[:, matchup[1:end .!= knap]]

            for i in 1:n_rounds
                # win
                if (player_knap[i] > maximum(opp_knaps[i,:])) || (player_knap[i] == maximum(opp_knaps[i,:]) && any(player_knap[i] .> opp_knaps[i,:]))
                    n_wins[knap] = n_wins[knap] + 1
                end
                if player_knap[i] == maximum(opp_knaps[i,:])
                    n_ties[knap] = n_ties[knap] + 1
                end
            end
        end

        # Now estiamte utility for each and enter in output matrix
        for knap in 1:length(matchup)
            current_matchup = vcat(matchup[knap], matchup[1:end .!= knap])
            if sum(n_wins) == 0
                out[current_matchup...] = 0
            elseif n_wins[knap] == maximum(n_wins)
                out[current_matchup...] = (n_wins[knap]/n_rounds) * ((n_players/sum(n_wins .== maximum(n_wins))) - 1) - (1 - (n_wins[knap]/n_rounds))
            elseif n_wins[knap] == minimum(n_wins)
                out[current_matchup...] = (n_wins[knap]/n_rounds) * ((n_players/sum(n_wins .== minimum(n_wins))) - 1) - (1 - (n_wins[knap]/n_rounds))
            else
                out[current_matchup...] = (n_players-1)*(n_wins[knap]/n_rounds) - (1 - (n_wins[knap]/n_rounds))
            end
        end
    end
    return out
end

# Tests ------------------------------------------------------------------------
estimate_knapsack_utility(collect(hcat([1,1,1,10], [2,2,2,2], [3,3,3,3])), 3)
estimate_knapsack_utility(collect(hcat([1,1,1,10], [2,2,2,2], [3,3,3,3],
                                        [5,5,5,5], [6,6,6,6], [7,7,7,7],
                                        [8,8,8,8], [9,9,9,9], [10,10,10,10])), 5)
