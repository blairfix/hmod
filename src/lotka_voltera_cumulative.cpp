#include <RcppArmadillo.h>
#include <iostream>
#include <math.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]

arma::mat   lotka_volt_cumulative(double r_0,
                                   double c_0,
                                   double k_1,
                                   double k_2,
                                   double k_3_init,
                                   double k_3_slope,
                                   double time,
                                   double step_size)

{

    int n_steps = std::round(time / step_size);

    // output matrix
    arma::mat output(n_steps, 4);


    // rescale params
    k_1 = k_1 * step_size;
    k_2 = k_2 * step_size;
    k_3_init = k_3_init * step_size;

    // initial conditions
    double r = r_0;
    double c = c_0;


    for(int i = 0; i < n_steps; i++){

        double r_frac = r / r_0;

        double k_3 = k_3_init * exp( - r_frac * k_3_slope );


        // deltas
        double delta_r = 0 - k_1 * c * r;
        double delta_c = k_2*c*r - k_3*c;

        // new values
        r = r + delta_r;
        if(r < 0){ r = 0;}

        c = c + delta_c;
        if(c < 0){ c = 0;}

        // export
        output(i, 0) = i*step_size;
        output(i, 1) = r;
        output(i, 2) = -delta_r;
        output(i, 3) = c;

    }

    return output;

}
