#include <RcppArmadillo.h>




// boot_span is a function to bootstrap from an empirical relation between the
// span of control s, and hierarchal level h. The function first samples pairs
// of data from s and h. It then uses the Armadillo solve function to get
// coefficients for a fitted exponential function. The output is span coefficients
// a and b.



// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]

arma::vec  boot_span(arma::vec h, arma::vec s)
{

    int n = s.size();



    // sample from span of control data
    int index;          // sample index
    arma::mat X(n, 2) ; // design matrix for linear regression
    arma::vec y(n) ;    // response vector for linear regression

    std::random_device rd;  //obtain a seed for the random number engine
    std::mt19937 gen(rd()); //Standard mersenne_twister_engine seeded with rd()
    std::uniform_real_distribution<> dis(0, 1);


    for(int i = 0; i < n; i++){
      index = n*dis(gen) ;
      X(i, 0) = 1;
      X(i, 1) = h[index];
      y[i] = log( s[index] );
    }

  arma::vec coef = arma::solve(X, y); // coefficients of regression

  coef[0] = exp(coef[0]);


  return coef;

}

