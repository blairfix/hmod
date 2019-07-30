#include <RcppArmadillo.h>
#include <algorithm>
#include <math.h>
#include <random>
#include <vector>

#include "sample_index.h"

//#define ARMA_DONT_USE_WRAPPER
////#define ARMA_NO_DEBUG
//#include <armadillo>

using namespace std;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]



// k_func calculates capitalist income for individuals


arma::vec k_function(arma::vec pay, arma::vec power, arma::mat k_parameters)
{

    // sample 1 set of values from k_parameters
    arma::uvec sample_id = sample_index(k_parameters.col(0), 1);

    double k_slope = k_parameters(sample_id[0], 0);
    double k_intercept = k_parameters(sample_id[0], 1);

    double cov_slope = k_parameters(sample_id[0], 2);
    double cov_intercept = k_parameters(sample_id[0], 3);



    // get capitalist income
    int n_people = pay.size();
    arma::vec k_income(n_people);

    std::default_random_engine generator;
    std::normal_distribution<double> distribution(0, 1);

    for(int i = 0; i < n_people; ++i){

        // capitalist fraction of income
        // linear function of hierarchical power
        // defined by k_slope and k_intercept
        double k_frac =  k_slope*std::log( power[i] ) + k_intercept;


        // add dispersion to k_frac,
        // coefficient of variation is a power-law function of hierarchical power
        // defined by cov_slope and cov_intercept
        double cov =  cov_intercept*std::pow( power[i], cov_slope);
        double sd = abs(cov*k_frac);

        // generate truncated normal distribution, max = 1
        double dispersion_max = 1 - k_frac;
        double dispersion = 2;

        while( dispersion > dispersion_max){
            dispersion = sd*distribution(generator);
        }

        k_frac = k_frac + dispersion;


            // fix k_frac values less than 0 or greater than 1
            if(k_frac < 0){
                k_frac = 0;
            }


        // total capitalist income
        k_income[i] = pay[i] * k_frac;

    }


    return k_income;

}


