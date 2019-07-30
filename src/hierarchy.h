//File: hierarchy.h
#ifndef HIERARCHY_H
#define HIERARCHY_H


#include <vector>
#include <math.h>
#include <algorithm>

#define ARMA_DONT_USE_WRAPPER
#define ARMA_NO_DEBUG
#define ARMA_DONT_USE_HDF5
#include <armadillo>


/*
hierarchy_func is the core model used to construct firm employment hierarchies.
Inputs are firm employment (emp), the size of the base hierarchical level (base),
and the vector sprod which contains the cumulative product of the spans of control
in all levels.

The algorithm first constructs a hierarchy and then tests if the resulting
total employment of the model matches the empirical data. If not, employees are
added/subtracted from the base. However, in the case where subtracting the error
from the base would result in a zero or negative value, employees are subtracted sequentially
from top levels until the correct total employment is reached.

The index for the first hierarchical level with zero employees is stored in h[19]
for further reference.
*/



arma::uvec hierarchy_func(const int &emp, const double &base, const arma::vec &sprod){

    // hierarchical employment vector
    arma::uvec h = arma::zeros<arma::uvec>(20);


    // set base level
    h[0] = (int) base;

    // correct to allow firm size of 1
    if( h[0] < 1 ){ h[0] = 1 ;}

    int j = 0;
    while(h[j] != 0){
        j++;
        h[j] =  (int)  base / sprod[j-1];
    }

    int maximum = j;


    // check for employment over/undershoot
    int check = arma::sum(h) - emp;


    // if h[0] - check <= 0 subtract overshoot from top levels
    // otherwise add/subtract to bottom level
    if( h[0] - check <= 0 ) {
        while (check > 0){
            h[maximum - 1] = h[maximum - 1] - 1;
            check =  check -1;
            if(h[maximum - 1] == 0 ){ maximum = maximum - 1; }
        }
    }
    else {
        h[0] = h[0] - check;
    }


    h[19] = maximum;


    return h;

}






#endif
