#include <RcppArmadillo.h>
#include "sample_replace.h"


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]


arma::vec boot_mean( arma::vec x, double conf, double n_boot)
{

    arma::vec mean_boot(n_boot);

    for(int i = 0; i < n_boot; i++){
        arma::vec x_sample = sample_replace(x, x.size());
        mean_boot[i] = arma::mean(x_sample);
    }

    arma::vec mean_boot_sort = arma::sort(mean_boot);

    // confidence intervals
    double alpha = 1 - conf;
    double lower_index = n_boot*( alpha / 2 );
    double upper_index = n_boot*( 1 - alpha / 2 );

    arma::vec output(3);
    output[0] = mean_boot_sort[lower_index];
    output[1] = arma::mean(x);
    output[2] = mean_boot_sort[upper_index];

    return output;
}


