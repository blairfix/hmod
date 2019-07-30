#include <RcppArmadillo.h>
#include <boost/math/special_functions/erf.hpp>
using namespace std;
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(BH)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]

double  boot_sigma(NumericVector x) 
{
  
  int n = x.size();

  NumericVector samp(n);
  int index;
  RNGScope scope;
  
    for(int i = 0; i < n; i++){
        index = n*unif_rand() ;
        samp[i] = x[index];
    }
  
  double m = mean(samp);

  double sigma =  2*boost::math::erf_inv(m);
  
  return sigma;
  
} 


