#include <RcppArmadillo.h>
#include <math.h>
#include "exponents.h"
#include "hierarchy.h"
#include "hierarchical_power.h"


// This function fits a power-income exponent beta to empirical firms.
// The inputs are:
//
//     a                   --- span of control parameter 1
//     b                   --- span of control parameter 2
//     base_employment vec --- vector of employment on rank 1 of each firm
//     total_employment_vec--- vector of total employment for each firm in base_employment_vec
//     ceo_ratio_vec       --- a vector of ceo pay ratios for the firms above
//     firm_mean_pay       --- a vector of mean pay in each firm
//     ceo_ratio_error_tollerance --- error tolerance for model fit
//     min_beta            --- left bound of beta
//     max beta            --- right bound of beta
//
//
// The model uses the bisector method to minimize the error betweened the modelled and empirical
// ceo ratio. The bisection method works by testing the sign of the error function at two different values of beta.
// The error function used is simply the difference between modelled and empirical
// ceo-to-employee pay ratios. We start with left bound of min_beta and right bound of max_beta
//
// We first get the error values at these bounds, and then the error at
// the bisection of these bounds (the first guess). We then look for
// the interval that contains a sign change, and thus contains the root.
//
// The structure of this code consists of a for loop over all firms.
// Within this loop, we have a while loop that implements the bisector method,
// until the desired error tolerance is achieved, or the maximum iterations occurs.


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]


arma::mat   fit_beta  ( double a,
                        double b,
                        const arma::vec &base_employment_vec,
                        const arma::uvec &total_employment_vec,
                        const arma::vec &ceo_ratio_vec,
                        const arma::vec &firm_mean_pay_vec,
                        double ceo_ratio_error_tolerance,
                        double min_beta,
                        double max_beta
                        )

{

    int   n_firm = base_employment_vec.size(); // number of firms
    arma::mat  output(n_firm, 3);       // output matrix


    arma::vec sprod = s_func(a, b);  // get span product

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // loop over firms

    for(int firm_index = 0; firm_index < n_firm; firm_index++){

        double base = base_employment_vec[firm_index];
        double c_r = ceo_ratio_vec[firm_index];
        int emp = total_employment_vec[firm_index];

        // get hierarchical power of each rank
        arma::uvec hierarchy_vec = hierarchy_func(emp, base, sprod); // get vector of employment by hierarchical level
        int maximum = hierarchy_vec[19]; // get index of maximum level
        arma::vec hierarchical_power_vec = hierarchical_power_function(hierarchy_vec);


        // *********************************************************************
        // beta optimization using bisection method

            // beta is a vector of power-income exponents
            // beta[0] = left bound
            // beta[1] = right bound
            // beta[2] =  current test value
            arma::vec beta(3);
            beta[0] = min_beta; // set initial left bound
            beta[1] = max_beta; // set initial right bounds

            arma::vec error(3);     // contain error corresponding to each value in beta vector
            double m_pay_model;     // estimate for firm mean pay
            int best_error = 100;   // set initial model error to high number to enter while loop
            int while_counter = 1;  // number of times through the while loop


        // test r value until error under tolerance or max iterations exceeded
        while (best_error > ceo_ratio_error_tolerance && while_counter < 50 ){

            // if first time through while loop, we run model
            // for left and right bounds, beta[0] and beta[1]
            // all other times we test only current guess, beta[2]
            // beta_start and beta_stop determine what values are tested

                int beta_start;
                int beta_stop;

                if(while_counter == 1){
                    beta_start = 0;
                    beta_stop = 2;
                } else {
                    beta_start = 2;
                    beta_stop = 3;
                }


            // loop over beta to get error for appropriate bounds
            for (int beta_index = beta_start; beta_index < beta_stop; beta_index++){

                // get mean pay of each  hierarchical rank
                arma::vec mean_pay_vec(maximum);

                for (int j = 0; j < maximum; j++){
                  mean_pay_vec[j] = pow( hierarchical_power_vec[j] , beta[beta_index] ) ;
                }


                // calculate firm mean pay (weighted mean of mean_pay by hierarchy vectors)
                double total = 0;
                double total_w = 0;

                for(int i = 0; i < maximum; ++i){
                  total += mean_pay_vec[i] * hierarchy_vec[i];
                  total_w += hierarchy_vec[i];
                }

                m_pay_model =  total / total_w;                             // mean pay of model
                double  ceo_ratio = mean_pay_vec[maximum-1]/m_pay_model;    // get model ceo pay ratio
                error[beta_index] =  c_r - ceo_ratio;                       // model error

            }



            ///////////////////////////////////////////////////////////////////////
            // bisection decision
            // if not first loop ... get new bounds
            // first check if left bound and test value are opposite signs
            // if yes ... beta[2] is new right bound
            // if no ... beta[2] is new left bound

                if (while_counter > 1){
                    if( error[0]*error[2] < 0.0 ){
                      beta[1] = beta[2];
                      error[1] = error[2];
                    }
                    else  {
                      beta[0] = beta[2];
                      error[0] = error[2];
                    }
                }

                beta[2] = (beta[0] +beta[1])/2;             // get new guess value
                best_error = std::abs(error[1]);   // current best error = error at midpoint
                while_counter++;

         }

    output(firm_index, 0) = beta[1];                                        // fitted pay scaling parameter
    output(firm_index, 1) = best_error;                                     // model error
    output(firm_index , 2) = firm_mean_pay_vec[firm_index] / m_pay_model;   // firm level base pay

    }


    return output;
}


