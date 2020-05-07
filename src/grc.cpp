#include <RcppArmadillo.h>


// Calculates the global reaching centrality, a measure of the
// degree of hierarchy in a network. See:
//
// Mones, E., Vicsek, L., & Vicsek, T. (2012).
// Hierarchy measure for complex networks. PloS one, 7(3).
//
// Input is a vector of the number of subordinates (reachable nodes)
// below each individual


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]


double grc(arma::vec n_subordinates)
{
    // reaching centrality (cr) of each person
    int n_people =  n_subordinates.size();
    arma::vec cr = n_subordinates / (n_people - 1);

    // max reaching centrality
    double cr_max = arma::max(cr);

    // global reaching centrality
    double numerator = arma::sum(cr_max - cr);
    double grc = numerator / (n_people - 1);

    return grc;
}

