//File: sample_replace.h
#ifndef SAMPLE_REPLACE_H
#define SAMPLE_REPLACE_H

#define ARMA_DONT_USE_WRAPPER
#define ARMA_NO_DEBUG
#include <armadillo>
#include <random>

// Takes a sample of size "size" out of vector x with replacement.
// Code is taken from the RcppArmadillo sample.h template:


arma::vec sample_replace(const arma::vec &x, int size)
{

    std::random_device rd;  
    std::mt19937 gen(rd()); 
    std::uniform_real_distribution<> dis(0, 1);

    arma::vec ret(size);
    int nOrig = x.size();
    arma::uvec sub(nOrig);

    for(int i = 0; i < size; i++){
        int index = nOrig * dis(gen);
        ret[i] = x[index];
    }

    return ret;

}

#endif
