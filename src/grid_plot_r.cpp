#include <RcppArmadillo.h>
#include <algorithm>
#include <random>
#include <math.h>
#include "exponents.h"

using namespace std;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(BH)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]


arma::mat grid_plot_r(
                    double a,
                    double b,
                    double sigma,
                    arma::umat firm_grid,
                    arma::vec base_vec,
                    arma::vec base_pay_vec,
                    arma::vec r_vec
                    )

{

    arma::mat coord_all(0, 4);

    int n_firms = base_vec.size();
    arma::vec sprod = s_func(a, b);
    arma::uvec e_pay = p_func(a, b);


    for(int i_firm = 0; i_firm < n_firms; ++i_firm){


        double   x    = firm_grid(i_firm, 1);
        double  x_end = firm_grid(i_firm, 2);
        double  y     = firm_grid(i_firm, 3);
        double  y_end = firm_grid(i_firm, 4);
        double  base  = base_vec[i_firm];


        int level = 1;
        bool not_max = true;

        while(not_max && level < 19){

            // grid_get
            if(base < 1 & level == 1){

                arma::mat coord_firm(1,4);

                coord_firm(0,0) = i_firm;
                coord_firm(0,1) = x;
                coord_firm(0,2) = y;
                coord_firm(0,3) = 1;
                coord_all.insert_rows(coord_all.n_rows, coord_firm);

            } else{

                double n;

                    if(level == 1){
                        n = base;
                    }
                    else {
                        n = base/sprod[level-1];
                    }

                int side = round(sqrt(n));

                if(side == 0){
                    not_max = false;}
                else{

                    arma::mat coord_firm(pow(side, 2), 4);

                    double x_mid = (x + x_end)/2.0 -  (1.0 + side)/2.0;
                    double y_mid = (y + y_end)/2.0 -  (1.0 + side)/2.0;

                    for(int i = 0; i < side; ++i){
                        for(int j = 0; j < side; j++){

                            coord_firm(side*i + j, 0) = i_firm;
                            coord_firm(side*i + j, 1) = 1 + j + x_mid;
                            coord_firm(side*i + j, 2) = 1 + i + y_mid;
                            coord_firm(side*i + j, 3) = level;

                        }
                    }

                    coord_all.insert_rows(coord_all.n_rows, coord_firm);

                }
            }
            ++ level;
        }

    }


    // individual pay
    arma::vec pay_vec(coord_all.n_rows);
    double mu =  - 0.5*pow(sigma, 2);

    random_device rd;
    mt19937 gen(rd());
    lognormal_distribution<> d(mu, sigma);

    for(int i = 0; i < coord_all.n_rows; ++i){

        int firm_index = coord_all(i,0);
        int level = coord_all(i,3) + 1;
        double r = r_vec[firm_index];
        double base_pay = base_pay_vec[firm_index];
        pay_vec[i] = base_pay*pow(r, e_pay[level])*d(gen) + 1;

    }

    coord_all.insert_cols(coord_all.n_cols, pay_vec);

    return coord_all;

}


