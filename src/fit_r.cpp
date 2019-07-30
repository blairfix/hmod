#include <RcppArmadillo.h>
#include "exponents.h"
#include "hierarchy.h"


// fit_r is a function that fits a hierarchical pay structure to empirical firms.
// The inputs are span of control parameters a and b, a vector of base level employment
// in different firms (base_emp_vec), a vector of total employment for the same firms
// (emp_vec) a vector of ceo-to-employee pay ratios for the same firms (c_r_vec), and a
// vector of mean pay (m_pay_vec), again for the same firms. Lastly, the variable tol
// sets the error tolerence for how closely the model should replicated the ceo pay ratio.
// The model uses the bisector method to minimize the error betweened the modelled and empirical
// ceo ratio.
//
// The bisection method works by testing the sign of the error function at two different pay
// scaling values (r). The error function used is simply the difference between modelled and empirical
// ceo-to-employee pay ratios. We start with an initial bound for the solution, chosen to be pay-scaling
// values r = 1 (left bound) and r = 1.7 (right bound). We first get the error values at these bounds,
// and then the error at the bisection of these bounds (the first guess). We then look for
// the interval that contains a sign change, and thus contains the root.
//
//
// The structure of this code is a bit messier than might be desired, but consists of a
// for loop over all firms. Within this loop, we have a while loop that implements the bisector method,
// until the desired error tolerance is achieved, or the maximum iterations occurs.


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]



arma::mat   fit_r  (double a,
                        double b,
                        const arma::vec &base_emp_vec,
                        const arma::uvec &emp_vec,
                        const arma::vec &c_r_vec,
                        const arma::vec &m_pay_vec,
                        double tol
                        )

{

    int   n_firm = base_emp_vec.size(); // number of firms
    arma::mat  output(n_firm, 3);       // output matrix


    arma::vec sprod = s_func(a, b);  // get span product
    arma::uvec e_pay = p_func(a, b); // get pay exponent

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // loop over firms

    for(int firm_index = 0; firm_index < n_firm; firm_index++){

        double base = base_emp_vec[firm_index];
        double c_r = c_r_vec[firm_index];
        int emp = emp_vec[firm_index];

        // employment model
        arma::uvec h = hierarchy_func(emp, base, sprod); // get vector of employment by hierarchical level
        int maximum = h[19]; // get index of maximum level


        // *********************************************************************
        // pay scaling (r) optimization using bisection method

            // r is a vector of pay scaling parameters
            // r[0] = left bound
            // r[1] = right bound
            // r[2] =  current test value
            arma::vec r(3);
            r[0] = 1.0; // set initial left bound
            r[1] = 1.7; // set initial right bounds

            arma::vec error(3);     // contain error corresponding to each value in r vector
            double m_pay_model;     // estimate for firm mean pay
            int best_error = 100;   // set initial model error to high number to enter while loop
            int while_counter = 1;  // number of times through the while loop


        // test r value until error under tolerance or max iterations exceeded
        while (best_error > tol && while_counter < 50 ){

            // if first time through while loop, we run model
            // for left and right bounds, r[0] and r[1]
            // all other times we test only current guess, r[2]
            // r_start and r_stop determine what values are tested

                int r_start;
                int r_stop;

                if(while_counter == 1){
                    r_start = 0;
                    r_stop = 2;
                } else {
                    r_start = 2;
                    r_stop = 3;
                }


            // loop over r to get error for appropriate bounds
            for (int r_index = r_start; r_index < r_stop; r_index++){

                // get mean pay of each level
                arma::vec p(maximum);

                for (int j = 0; j < maximum; j++){
                  p[j] = pow( r[r_index], e_pay[j]) ;
                }


                // calculate firm mean pay (weighted mean of p by h vectors)
                double total = 0;
                double total_w = 0;

                for(int i = 0; i < maximum; ++i){
                  total += p[i] * h[i];
                  total_w += h[i];
                }

                m_pay_model =  total / total_w;               // mean pay of model
                double  ceo_ratio = p[maximum-1]/m_pay_model; // get model ceo to employee pay ratio
                error[r_index] =  c_r - ceo_ratio;            // model error

            } // end for r loop



            ///////////////////////////////////////////////////////////////////////
            // bisection decision
            // if not first loop ... get new bounds
            // first check if left bound and test value are opposite signs
            // if yes ... r[2] is new right bound
            // if no ... r[2] is new left bound

                if (while_counter > 1){
                    if( error[0]*error[2] < 0.0 ){
                      r[1] = r[2];
                      error[1] = error[2];
                    }
                    else  {
                      r[0] = r[2];
                      error[0] = error[2];
                    }
                }

                r[2] = (r[0] +r[1])/2;             // get new guess value
                best_error = std::abs(error[1]);   // current best error = error at midpoint
                while_counter++;

         }

    output(firm_index, 0) = r[1];                                 // fitted pay scaling parameter
    output(firm_index, 1) = best_error;                           // model error
    output(firm_index , 2) = m_pay_vec[firm_index] / m_pay_model; // firm level base pay

    }


    return output;
}



