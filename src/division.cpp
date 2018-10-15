

#include <Rcpp.h> 
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector divisionCpp(NumericVector v, NumericVector w, int n)
{
  NumericVector z(n); 
  for(int i = 0; i <= n; i++)
  {
    z[i] = v[i]+w[i];
  }
  return z; 
}
