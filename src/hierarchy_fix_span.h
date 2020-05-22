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
    int n_levels = floor( log(emp*(span-1)+1)/log(span) );

    // get size of bottom rank
    double base =  emp*( 1 - 1/span )/( 1 - std::pow(1/span, n_levels) ) ;

    // hierarchical employment vector
    // (leave exta elements at end of vector for possible overshoot
    int max_h = n_levels + 3;
    arma::vec h = arma::zeros<arma::vec>( max_h );

    // correct base to allow firm size of 1
    h[0] = floor( base );
    if( h[0] < 1 ){ h[0] = 1 ;}

    int i = 1;
    bool stop = false;

    while( stop == false ){

        int level = floor( base / std::pow(span, i) );
        h[i] = level;

        if( level == 0){

            max_rank = i;
            stop = true;

        } else if( i == max_h - 1){

            max_rank = i + 1;
            stop = true;

        }

        i++;

    }

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
