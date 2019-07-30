#include <RcppArmadillo.h>



// gini calculates the gini index of the vector x. If corr = true,
// the gini is corrected for small sample bias. Code is based off
// of the gini function contained in the R 'ineq' package

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]


double gini(arma::vec x, bool corr = false)
{


    int n = x.size();

    std::sort(x.begin(), x.end());
    double G = 0;

    for(int i = 0; i < n; i++){
        G = G + x[i]*(i+1);
    }

    double sum = arma::sum(x);

    G = 2*G/sum - (n + 1);

        if(corr){
          G = G/(n-1);
        } else {
          G = G/n;
        }

    if(x.size() == 1){G = 0;}

    return G;
}


