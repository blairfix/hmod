#include <RcppArmadillo.h>
#include <algorithm>


// top_frac calculates the income share of the desired top fraction of incomes.
// Inputs:
//     pay = a vector of individual pay;
//     frac = the desired top income fractile


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]


double   top_frac( arma::vec pay, double frac)
{
    int n = pay.size();
    int k = frac*n;

    double total = std::accumulate(pay.begin(), pay.end(), 0.0);

    std::nth_element(pay.begin(), pay.end() - k, pay.end());

    double top = 0;

    for(int i = n- k; i < n; i++){
        top = top + pay[i];
    }


    double output = top/total;

    return output;
}


