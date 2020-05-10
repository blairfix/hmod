#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]

double weighted_mean( arma::vec x, arma::vec weights)
{

    if( x.size() != weights.size() ){
        throw std::range_error("weights must have the same length as x");
    }

	double total = 0;
	double total_weight = 0;

	for(int i = 0; i < x.size(); ++i){

		total          += x[i] * weights[i];
		total_weight   += weights[i];

	}

	double m =  total / total_weight;

	return m;
}



