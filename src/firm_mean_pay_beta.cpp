#include <RcppArmadillo.h>
#include <math.h>
#include "exponents.h"
#include "hierarchy.h"
#include "hierarchical_power_v1.h"

// a function to find the average pay in a modeled firm


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]

arma::vec firm_mean_pay(    double a,
                            double b,
                            const arma::vec &base_employment_vec,
                            const arma::uvec &total_employment_vec,
                            const arma::vec &base_pay_vec,
                            const arma::vec &beta_vec
                        )

{

    int   n_firm = base_employment_vec.size(); // number of firms
    arma::vec  firm_mean_pay_vec(n_firm);       // output matrix

    arma::vec sprod = s_func(a, b);  // get span product

    for( int firm_index = 0; firm_index < n_firm; firm_index++){

        // firm data
        double base = base_employment_vec[firm_index];
        int emp = total_employment_vec[firm_index];
        double base_pay = base_pay_vec[firm_index];
        double beta = beta_vec[firm_index];

        // employment hierarchy
        arma::uvec hierarchy_vec = hierarchy_func(emp, base, sprod); // get vector of employment by hierarchical level
        int maximum = hierarchy_vec[19]; // get index of maximum level
        arma::vec hierarchical_power_vec = hierarchical_power_function(hierarchy_vec);


        // mean pay of each  hierarchical rank
        arma::vec mean_pay_vec(maximum);

        for (int i = 0; i < maximum; i++){
          mean_pay_vec[i] = pow( hierarchical_power_vec[i] , beta ) ;
        }


        //  firm mean pay relative to base level
        double total_pay = 0;
        double total_weight = 0;

        for(int i = 0; i < maximum; ++i){
          total_pay += mean_pay_vec[i] * hierarchy_vec[i];
          total_weight += hierarchy_vec[i];
        }

        firm_mean_pay_vec[firm_index] = total_pay / total_weight * base_pay;

    }


    return firm_mean_pay_vec;
}



