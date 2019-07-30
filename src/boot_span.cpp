#include <RcppArmadillo.h>
using namespace std;
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]

NumericVector  boot_span(arma::vec h, arma::vec s) 
{
  
  int n = s.size();

  arma::mat X(n, 2) ; // design matrix
  arma::vec y(n) ;    // response vector
  
  int index;          // sample index
  RNGScope scope;
    
    for(int i = 0; i < n; i++){
      index = n*unif_rand() ;
      X(i, 0) = 1;
      X(i, 1) = h[index];
      y[i] = log( s[index] );
    }
  
  arma::vec coef = arma::solve(X, y); 
  
  coef[0] = exp(coef[0]);
  
  NumericVector output = wrap(coef);
  output.attr("dim") = R_NilValue;
  
  
  return output;
  
} 

