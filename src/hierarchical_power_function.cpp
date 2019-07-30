#include <RcppArmadillo.h>

// This function calculates the average hierarchical power
// of individuals in the input hierarchy_vec.
// The input vector contains aggregate employment by hierarchical rank.


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]


arma::vec hierarchical_power( arma::uvec hierarchy_vec )
{

    int maximum_rank = hierarchy_vec[19];
    double n_subordinate = 0;

    arma::vec mean_hierarchical_power(maximum_rank);


    //loop over levels in firm
    for (int i = 0; i < maximum_rank; ++i){

        mean_hierarchical_power[i] = n_subordinate / hierarchy_vec[i] + 1;

        n_subordinate = n_subordinate + hierarchy_vec[i];

    }

    return(mean_hierarchical_power);


}



