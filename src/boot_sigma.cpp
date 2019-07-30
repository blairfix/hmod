#include <RcppArmadillo.h>
#include <boost/math/special_functions/erf.hpp>




// boot_sigma is a function to bootstrap sample the vector x, a vector
// of gini indexes of pay inequality within different hierarchical levels of
// case study firms. The function first samples (with replacement) from this vector
// and then calculates the average of the bootstrap sample. This average value
// is then converted into the corresponding lognormal parameter sigma, which
// would produce a lognormal distribution with an identical gini index.


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(BH)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]


double  boot_sigma(arma::vec x)
{

    // sample with replacement from x
    // (the case study intra-hierarchical level gini index of pay
    int n = x.size();

    arma::vec samp(n);
    int index;


    std::random_device rd;  //Will be used to obtain a seed for the random number engine
    std::mt19937 gen(rd()); //Standard mersenne_twister_engine seeded with rd()
    std::uniform_real_distribution<> dis(0, 1);


    for(int i = 0; i < n; i++){
        index = n*dis(gen) ;
        samp[i] = x[index];
    }

    double m = arma::mean(samp); // get mean of sample  gini

    double sigma =  2*boost::math::erf_inv(m); // convert gini to lognormal sigma parameter

    return sigma;

}

