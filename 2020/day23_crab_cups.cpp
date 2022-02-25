#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector crabCupsCpp(IntegerVector input, IntegerVector nxt, int turns, int size) {
  int curr_id = 0;
  int maxval = size;
  int curr_val = input[curr_id];
  for (int i = 0; i<turns; ++i) {
    int pick1 = nxt[curr_val];
    int pick2 = nxt[pick1];
    int pick3 = nxt[pick2];

    int dest_val = (curr_val == 1) ? maxval : curr_val-1;
    while (dest_val == pick1 || dest_val == pick2 || dest_val == pick3) {
      dest_val = (dest_val == 1) ? maxval : dest_val-1;
    }
    nxt[curr_val] = nxt[pick3];
    nxt[pick3] = nxt[dest_val];
    nxt[dest_val] = pick1;
    curr_val = nxt[curr_val];
  }
  return nxt;
}
