#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double test(NumericMatrix item_draws) {
  double total = 0;
  int n_knaps = item_draws.ncol();
  int n_rounds = item_draws.nrow();
  
  for (int p1 = 0; p1 < n_knaps; p1++) {
    for (int p2 = 0; p2 < n_knaps; p2++) {
      for (int round = 0; round < n_rounds; round++) {
        
      }
    }
  }
  
  return total;
}

/*** R
test(matrix(c(1,2,3,4), ncol = 2))
*/
