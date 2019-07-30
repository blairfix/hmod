#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]


double weighted_mean( NumericVector x, NumericVector w)
{

	double total = 0;
	double total_w = 0;
		
	for(int i = 0; i < x.size(); ++i){

		total += x[i] * w[i];
		total_w += w[i];

	}

	double m =  total / total_w;
		
	return m;
}



