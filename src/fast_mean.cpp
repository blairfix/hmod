#include <Rcpp.h>
using namespace std;
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]


  double fast_mean( NumericVector x) 
  {
    
    double m = sum(x)/x.size();
    
    
    return m;
  }   



