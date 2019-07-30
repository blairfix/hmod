#include <RcppArmadillo.h>

// This function generates a Lorenz curve from the vector 'pay'.
// Lorenz curve values are calculated for a logarithmically spaced vector from 'lower' to 'upper',
// with 'n_bins' indicating the number of points.

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]


arma::mat lorenz(   arma::vec pay,
                    double lower,
                    double upper,
                    int n_bins
)
{


    // normalize pay to empirical mean
    int n_people = pay.size();
    double mean_pay = arma::mean(pay);

    for(int i = 0; i < n_people; ++i){
        pay[i] = pay[i] /mean_pay;
    }

    arma::vec bin_vec = arma::logspace( std::log10(lower), std::log10(upper), n_bins);

    // get lorenz
    arma::mat result(2, n_bins);

    int n = bin_vec.size();
    double total_income = arma::sum(pay);

    for(int i = 0; i < n; ++i){

        int person_counter = 0;
        double pay_counter = 0;
        double bin = bin_vec[i];

        for(int j = 0; j < n_people; j++){

            if(pay[j] <= bin){
                ++person_counter;
                pay_counter = pay_counter + pay[j];
            }
        }

        result(0,i) = (double) person_counter/ (double) n_people;
        result(1,i) = pay_counter / total_income;
    }


    return result;

}
