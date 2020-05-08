//File: hierarchy.h
#ifndef HIERARCHY_H
#define HIERARCHY_H

#define ARMA_DONT_USE_WRAPPER
#define ARMA_NO_DEBUG
#define ARMA_DONT_USE_HDF5
#include <armadillo>

/*
Creates a hierarchy from an inputed firm size vector.
The span of control is fixed for all hierarchical ranks.
*/


arma::vec hierarchy_func(const double &emp, const double &span, int & max_rank){

    // number of hierarchical levels in firm
    double n_levels = floor( log(emp*(span-1)+1)/log(span) );

    // hierarchical employment vector
    // (leave exta elements at end of vector for possible overshoot
    arma::vec h = arma::zeros<arma::vec>( n_levels + 2);

    // get size of bottom rank
    double base =  ceil( emp*( 1 - 1/span )/( 1 - std::pow(1/span, n_levels) ) );

    // correct to allow firm size of 1
    if( h[0] < 1 ){ h[0] = 1 ;}

    int i = 0;
    while( h[i] != 0 ){

        i++;
        h[i] =  floor( base / std::pow(span, i) );

    }

    max_rank = i;


    // check for employment over/undershoot
    int check = arma::sum(h) - emp;

    // if h[0] - check <= 0 subtract overshoot from top levels
    // otherwise add/subtract to bottom level
    if( h[0] - check <= 0 ) {
        while (check > 0){
            h[max_rank - 1] = h[max_rank - 1] - 1;
            check =  check -1;
            if(h[max_rank - 1] == 0 ){ max_rank = max_rank - 1; }
        }
    }
    else {
        h[0] = h[0] - check;
    }

    return h;

}

#endif
