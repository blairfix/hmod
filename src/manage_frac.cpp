#include <RcppArmadillo.h>
#include "hierarchy_fix_span.h"

// a function to model the management fraction of employment
// in a population of hierarchical firms

using namespace std;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]

double manager_frac(const arma::vec &firm_vec,
                    double span,
                    int manage_rank_thresh
                    )

{

    int n_firms = firm_vec.size();
    double managers_total  = 0;


    // get number of hierarchical levels and base employment in each firm
    arma::vec n_levels(n_firms);
    arma::vec base_emp(n_firms);


    // loop over firms
    for(int i = 0; i < n_firms; ++i ){

        // create the employment hierarchy in the firm
        double firm_emp = firm_vec[i];
        int max_rank;

        arma::vec hierarchy_vec = hierarchy_func(firm_emp, span, max_rank);

        // management employment
        // managers = employees in h_level > manage_rank_thresh
        double firm_managers;

        if(max_rank >= manage_rank_thresh){
            arma::vec manager_levels = hierarchy_vec.subvec(manage_rank_thresh - 1, max_rank - 1);
            firm_managers = arma::sum(manager_levels);
        } else{
            firm_managers = 0;
        }

        // running total of management employment
        managers_total = managers_total + firm_managers;
    }


    // employment share of managers
    double total_employment = arma::sum(firm_vec);
    double management_fraction = managers_total / total_employment;

    return management_fraction;
}
