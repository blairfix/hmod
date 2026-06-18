#include <RcppArmadillo.h>
#include <random>
#include <math.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]

arma::vec rplc(
	int n,
	double xmin,
	double alpha
	)
{

    // output vector
    arma::vec output(n);

    // setup random generator
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_real_distribution<> dis(0, 1.0);


    for( int i = 0; i < n; i++){

	// uniform random number
	double rand = dis(gen);

	// convert to power-law distribution
	output(i)= xmin * pow( (1 - rand ),  -1 / (alpha - 1) );

    }

    return output ;

}

