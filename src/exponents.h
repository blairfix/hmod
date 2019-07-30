//File: exponents.h
#ifndef EXPONENTS_H
#define EXPONENTS_H

#include <RcppArmadillo.h>
#include <math.h>

// s_func creates a vector (sprod) which is the the cumulative product of
// the span of control of each hierarchical level. Thus employment in
// any hierchical level i is simply base employment / sprod[i]


inline arma::vec s_func(double a, double b){


    // initialize and fill exponent vector e_emp
    arma::uvec e_emp(20);

    for (int i = 0; i < 20; ++i){
      e_emp[i] = i+2;
    }


    // span of control vector
    arma::vec s(20);

    // fill span vector with values s = a*e^b
    for (int i = 0; i < 20; ++i){
      s[i] = a*exp(b*e_emp[i]);
    }

    // cumulative product of span vector
    arma::vec sprod = arma::cumprod(s);

    return sprod;

}




// p_func creates a vector of exponents that is used for calculating hierarchial pay


inline arma::uvec p_func(double a, double b){


    //  initialize and fill exponent vector e_pay
    arma::uvec e_pay(20);

    for (int i = 0; i < 20; ++i){
      e_pay[i] = i;
    }

    // cumulative sum of e
    std::partial_sum(e_pay.begin(), e_pay.end(), e_pay.begin());

    return e_pay;

}


#endif
