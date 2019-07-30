//File: sample_index.h
#ifndef SAMPLE_INDEX_H
#define SAMPLE_INDEX_H

#include <random>

#define ARMA_DONT_USE_WRAPPER
#define ARMA_NO_DEBUG
#include <armadillo>


/*
Takes a sample of size "size" out of vector x without replacement.
Code is taken from the RcppArmadillo sample.h template:
https://github.com/RcppCore/RcppArmadillo/blob/master/inst/unitTests/cpp/sample.cpp
*/


arma::uvec sample_index( const arma::vec &x, int size)
{

    std::random_device rd;  //Will be used to obtain a seed for the random number engine
    std::mt19937 gen(rd()); //Standard mersenne_twister_engine seeded with rd()
    std::uniform_real_distribution<> dis(0, 1);


    arma::uvec index(size); // Store the sample ids here, modify in-place
    int ii, jj;
    int nOrig = x.size();
    arma::uvec sub(nOrig);


    for (ii = 0; ii < nOrig; ii++) {
        sub[ii] = ii;
    }
    for (ii = 0; ii < size; ii++) {
        jj = nOrig * dis(gen);
        index[ii] = sub[jj];
        // replace sampled element with last, decrement
        sub[jj] = sub[--nOrig];
    }


    return index;

}



#endif
