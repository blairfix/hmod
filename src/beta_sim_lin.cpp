#include <RcppArmadillo.h>
#include <math.h>
#include <random>
#include <vector>



// beta sim fits a lognormal distribution to
// the distribution of beta, the power-income exponent.
//
// mu is constant for all firms
//
// sigma is a power function of firm size,
// determined from regression on log-binned firm-size data.
//
//
// inputs:
//     employment      --- vector of firm employment
//     beta            --- vector of fitted beta values for each firm
//     sim_employment  --- vector of simulated values for firm employment
//     bin_factor      --- determines how many log-spaced firm size bins are used in regression
//
// output =  beta values for each simulated firm in sim_employment



// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(BH)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]


arma::vec beta_sim_linear(  const arma::uvec &employment,
                            const arma::vec &beta,
                            const arma::uvec &sim_employment,
                            double bin_factor  // greater = more employment bins
                )

{

    arma::vec emp = arma::conv_to<arma::vec>::from(employment); // convert employment to arma::vec


    // get lognormal location parameter, mu_beta
    double mu_beta =    arma::mean(arma::log(beta));


    // get lognormal scale parameter, sigma_beta
    // using regression on long-binned firm size

        // group firms into rounded log of employment
        arma::vec group =  arma::round( bin_factor*arma::log(emp) )/bin_factor ;
        arma::vec group_unique = arma::unique(group);
        int n_group = group_unique.size();
        arma::vec sigma_beta(n_group);

        // get sigma_beta within each firm size bin
        // sigma_beta = standard deviation of log beta
        for(int i = 0; i < n_group; ++i){

            arma::uvec ids = find(group == group_unique[i]);
            arma::vec beta_sub = beta.elem(ids);
            sigma_beta[i] = arma::stddev(arma::log(beta_sub));

        }

        // regression on sigma vs group (log of employment)
        arma::mat X(n_group, 2); // response matrix
        X.ones();
        X.col(1) = group_unique;
        arma::vec sigma_coef = arma::solve(X, sigma_beta );


    ///////////////////////////////////////////////////////////////////////////////////
    // generate simulated beta distribution
    // each firm gets sigma_beta based on employment
    // every firm gets same mu_beta

    std::random_device rd;
    std::mt19937 gen(rd());
    std::normal_distribution<> d(0, 1);

    int n_sim_firms = sim_employment.size();
    arma::vec beta_output(n_sim_firms);

    for(int i = 0; i < n_sim_firms; ++i){

        double sigma_firm =  sigma_coef[0] + sigma_coef[1]*std::log( sim_employment[i] ) ;

        beta_output[i] = std::exp(  sigma_firm*d(gen) + mu_beta);

    }


    return beta_output;
}


