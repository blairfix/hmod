#include <RcppArmadillo.h>
#include <boost/math/special_functions/digamma.hpp>
#include <boost/math/special_functions/polygamma.hpp>




// The base_pay_sim function creates a simulated distribution of firm pay in the
// base hierarchical level. It fits a gamma distribution to the vector of pay data
// contained in "base_pay_empirical". In the context of this model, base_pay_empirical is
// an estimate derived from the hierarchical modelling of compustat firms. The size of
// the output vector is determined by n_sim, the desired number of simulated firms.


// [[Rcpp::depends(BH)]]
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]

arma::vec base_pay_sim( const arma::vec &base_pay_empirical,
                        int n_sim)
{

    int n = base_pay_empirical.size(); // number of firms in emprical sample

    // Get initial guess for k
    double sum = 0;
    double sum_log = 0;


    for(int i = 0; i < n; ++i){
        sum = sum + base_pay_empirical[i];
        sum_log = sum_log + log(base_pay_empirical[i]);
    }

    double s = log(sum/n) - sum_log/n;

    double k = (3 - s + pow( pow(s-3, 2) + 24*s, 0.5  ) )/(12*s)  ;
    double k_new;
    double error = 1;

    // Newton method to estimate k
    while(error > 0.00001){

        k_new = k -   (log(k) - boost::math::digamma(k) -s) / (1/k  - boost::math::polygamma(1, k) )  ;
        error = abs(k_new - k);
        k = k_new;

    }

    double theta = 1/(k*n)*sum; // get other gamma parameter,theta


    // generate random gamma distribution for simulated firm base pay distribution
    arma::vec x(n_sim);

    std::random_device rd;  //Will be used to obtain a seed for the random number engine
    std::mt19937 gen(rd());
    std::gamma_distribution<double> distribution(k, theta);

    for(int i = 0; i < n_sim; ++i){
        x[i] = distribution(gen);
    }


    return x;

}



