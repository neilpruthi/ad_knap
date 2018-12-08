import numpy as np
import itertools as it
# from joblib import Parallel, delayed
from collections import ChainMap

def match_results(match):
    opp_max = np.amax(match[1:len(match)])
    actual_max = np.amax(match)
    # Loss
    if match[0] < opp_max:
        return(-1)
    # total win
    elif all(match[0] > match[1:len(match)]):
        return(len(match)-1)
    # splitwin
    else:
        return((len(match)-sum(match == actual_max))/(sum(match == actual_max)))
        
def est_knap_u(item_draws, n_players, parallel = False):
    n_rounds = item_draws.shape[0]
    n_knaps = item_draws.shape[1]
    
    if parallel:
        a = lambda match: dict({''.join(str(e) for e in match): np.mean(np.apply_along_axis(match_results, 1, item_draws[:, match]))})
        utility = Parallel(n_jobs=16)(delayed(a)(match) for match in it.product(range(n_knaps), repeat=n_players))
        # Convert list of dicts to single dict
        utility = dict(ChainMap(*utility))
    else:
        utility = {}
        for match in it.product(range(n_knaps), repeat=n_players):       
            utility[''.join(str(e) for e in match)] = np.mean(np.apply_along_axis(match_results, 1, item_draws[:, match]))

            
    return(utility)

# Ex
# test = np.array([[1,2,3,4], [1,2,3,4], [1,2,3,4], [1,2,3,4], [1,2,3,4], [1,2,3,4], [1,2,3,4]])
# testu = est_knap_u(test, 2, parallel = False)
# print(testu)
