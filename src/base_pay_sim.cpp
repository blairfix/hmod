#include <RcppArmadillo.h>
#include <boost/math/special_functions/digamma.hpp>
#include <boost/math/special_functions/polygamma.hpp>
using namespace std;
using namespace Rcpp;

// [[Rcpp::depends(BH)]]
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]


  NumericVector base_pay_sim(NumericVector base_empirical, int n_sim) 
  {
      int n = base_empirical.size();
      double s = log(mean(base_empirical)) - mean(log(base_empirical)) ;
      double k = (3 - s + pow( pow(s-3, 2) + 24*s, 0.5  ) )/(12*s)  ;
      double k_new;
      double error = 1;

    // Newton method
      while(error > 0.00001){
      
        k_new = k -   (log(k) - boost::math::digamma(k) -s) / (1/k  - boost::math::polygamma(1, k) )  ; 
        error = abs(k_new - k);
        k = k_new;
        
      }
      
      double theta = 1/(k*n)*sum(base_empirical);
    
    
    
    // random gamma
      arma::vec x(n_sim);
      
      x = arma::randg(n_sim, arma::distr_param(k, theta));
    
    return wrap(x);
  
  }   



