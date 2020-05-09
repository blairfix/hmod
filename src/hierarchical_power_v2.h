//File: hierarchical_power_v2.h
#ifndef HIERARCHICAL_POWER_V2_H
#define HIERARCHICAL_POWER_V2_H

#define ARMA_DONT_USE_WRAPPER
#define ARMA_NO_DEBUG
#define ARMA_DONT_USE_HDF5
#include <armadillo>

/*
This function calculates the average hierarchical power
of individuals in the input hierarchy_vec.
The input vector contains aggregate employment by hierarchical rank.
*/

inline arma::vec hierarchical_power_function(const arma::vec &hierarchy_vec, int max_rank )
{

    double n_subordinate = 0;

    arma::vec mean_hierarchical_power(max_rank);

    //loop over levels in firm
    for (int i = 0; i < max_rank; ++i){

        mean_hierarchical_power[i] = n_subordinate / hierarchy_vec[i] + 1;

        n_subordinate = n_subordinate + hierarchy_vec[i];

    }

    return(mean_hierarchical_power);

}

#endif
