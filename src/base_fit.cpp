#include <RcppArmadillo.h>
#include "exponents.h"
#include <math.h>


// NOTES:
// base_fit is a function that fits the base hierarchical employment for a firm, given
// total employment and span of control parameters a and b. The function
// takes single value inputs for a and b (parameters determining the span of control),
// and a vector of inputs for employment. The function works by making a trial vector of
// base levels and then uses the model formula to build the a hierarchy up from the base
// (for each firm in the trial vector). Total employment is then calculated for each trial firm.
// This gives a discrete mapping between base employment and total employment for the trial vector
// (if there are duplicated employment values for two different elements in the base trial vector,
// we eliminate them). We then use the C++ Armadillo interpolate function to get estimate base
// level values for each value in the input employment vector emp_vec.


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]



arma::vec base_fit( double a,
                    double b,
                    const arma::uvec &emp_vec)
{


    ////////////////////////////////////////////////////////////////////////////////////////////////////
    // initialize and fill trial base vector
    int n_trial = 10000; // length of base_trial
    int linear = 2000; // number of linear steps in base_trial

    arma::vec base_trial(n_trial);
    base_trial[0] = 0.02;

    // fill linear steps in base_trial
    for (int i = 1; i < linear; ++i){
        base_trial[i] = base_trial[i-1] + 0.05;
    }

    // check max of emp vec to inform max of base_trial vector
    int maximum  = emp_vec.max();
    if( maximum < 2*pow(10, 6) ){ maximum = 2*pow(10, 6); } // maximum not below 2 million


    // get r such that base_trial[linear-1]*r^(l-linear) = maximum
    double r = exp( (log(maximum) -  log(base_trial[linear-1])  )/ (n_trial -linear)  ) ;


    // fill the rest of base_trial with exponential sequence
    for (int i = linear; i < n_trial; ++i){
        base_trial[i] = base_trial[i-1]*r;
    }


    ///////////////////////////////////////////////////////////////////////////////////////////////////////
    // employment model

    // get span product and pay exponent
    arma::vec sprod = s_func(a, b);


    ////////////////////////////////////////////////////////////
    // loop over base_trial and get employment for each base
    arma::vec emp_trial(n_trial);

    for(int b_index = 0; b_index < n_trial; ++b_index){

        // base employment
        double base = base_trial[b_index];

        // hierarchical employment vector
        arma::uvec h(20);

        // set base level
        h[0] = base;

        // correct to allow firm size of 1
        if( h[0] < 1  ){ h[0] = 1 ;}

        // fill hierarchical levels
        for (int i = 1; i < 20; ++i){
            h[i] = base / sprod[i-1];
        }

        emp_trial[b_index] = arma::sum(h);

    }


    ///////////////////////////////////////////////////////////////////////////////////////
    // find unique, non-zero elements of emp_trial and match with corresponding base level

    arma::uvec indices = arma::find_unique( emp_trial, true );
    arma::vec emp_unique = emp_trial.elem(indices);
    arma::vec base_unique = base_trial.elem(indices);


    // model base and total employent vector used for regression:
    arma::uvec ids = arma::find(emp_unique >= 1); // Find indices
    arma::vec emp_model = emp_unique.elem(ids);
    arma::vec base_model = base_unique.elem(ids);


    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // predict base level using linear interpolation of emp_model and base_model points

    arma::vec emp_vec_convert = arma::conv_to<arma::vec>::from(emp_vec);

    arma::vec base_predict_vec;

    arma::interp1(emp_model, base_model, emp_vec_convert, base_predict_vec, "linear");



  return base_predict_vec;
}

