#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int rcppPlay(IntegerVector x, int turns) {
  int l = x.size();
  IntegerVector diff_time = IntegerVector(turns+1);
  for(int i = 0;i<l;i++) {
    diff_time[x[i]] = i+1;
  }
  int last = x[l-1];
  int tmp;
  for (int turn = l; turn < turns; turn++) {
    tmp = last;
    last = (diff_time[tmp]) ? (turn - diff_time[tmp]) : 0;
    diff_time[tmp] = turn;
  }
  return last;
}
