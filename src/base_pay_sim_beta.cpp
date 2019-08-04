#include <RcppArmadillo.h>
#include <random>



// [[Rcpp::depends(BH)]]
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]

arma::vec base_pay_sim_beta(const arma::vec &base_pay_empirical,
                            const arma::vec &beta_empirical,
                            const arma::vec &beta_simulated )
{

    int n_firm = base_pay_empirical.size(); // number of firms in emprical sample

    arma::vec log_base_pay = arma::log(base_pay_empirical);

    // regression on log base_pay vs beta
    arma::mat X(n_firm, 2); // response matrix
    X.ones();
    X.col(1) = beta_empirical;
    arma::vec beta_coef = arma::solve(X, log_base_pay );

    // get standard deviation of residuals
    arma::vec log_base_pay_predict = beta_coef[0] + beta_coef[1]*beta_empirical;
    arma::vec residuals = log_base_pay_predict - log_base_pay;

    double sigma = arma::stddev(residuals);

    ///////////////////////////////////////////////////////////////////////////////////
    // generate simulated base pay for each firm from lognormal distribution
    // each firm gets mu based on beta
    // every firm gets same sigma

    std::random_device rd;
    std::mt19937 gen(rd());
    std::normal_distribution<> d(0, 1);

    int n_sim_firms = beta_simulated.size();
    arma::vec base_pay_simululated(n_sim_firms);

    for(int i = 0; i < n_sim_firms; ++i){

        double mu_firm = beta_coef[0] + beta_coef[1]*beta_simulated[i] ;

        base_pay_simululated[i] = std::exp(  sigma*d(gen) + mu_firm);

    }


    return base_pay_simululated;

}



