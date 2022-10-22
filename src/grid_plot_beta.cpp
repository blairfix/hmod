#include <RcppArmadillo.h>
#include <algorithm>
#include "exponents.h"
#include "hierarchy.h"
#include "hierarchical_power_v1.h"
#include <math.h>
#include <random>


// plots firm hierarchies as pyramids on a grid
// output is a list of matrix of x, y, and z coordinates for individuals
// as well as the hierarchical rank, hierarchical power and income of these people.


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(BH)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]


arma::mat grid_plot_beta(
                    double a,
                    double b,
                    double sigma,
                    arma::umat firm_grid,
                    arma::vec emp_vec,
                    arma::vec base_vec,
                    arma::vec base_pay_vec,
                    arma::vec beta_vec
                    )

{

    arma::mat coord_all(0, 5);

    int n_firms = base_vec.size();
    arma::vec sprod = s_func(a, b);



    for(int i_firm = 0; i_firm < n_firms; ++i_firm){

        // firm employment by hierarchical rank and hierarchical power by rank
        arma::uvec hierarchy_vec = hierarchy_func(emp_vec[i_firm], base_vec[i_firm], sprod);
        int maximum_rank = hierarchy_vec[19];
        arma::vec hierarchal_power_vec = hierarchical_power_function(hierarchy_vec);


        double  x    = firm_grid(i_firm, 1);
        double  x_end = firm_grid(i_firm, 2);
        double  y     = firm_grid(i_firm, 3);
        double  y_end = firm_grid(i_firm, 4);
        double  base  = base_vec[i_firm];


        int level = 1;
        bool not_max = true;

        while(not_max && level < 19){

            // grid_get
            if(base < 1 & level == 1){

                arma::mat coord_firm(1,5);

                coord_firm(0,0) = i_firm;
                coord_firm(0,1) = x;
                coord_firm(0,2) = y;
                coord_firm(0,3) = 1;    // hierarchical rank
                coord_firm(0,4) = 1;    // hierarhcical power
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

                    arma::mat coord_firm(pow(side, 2), 5);

                    double x_mid = (x + x_end)/2.0 -  (1.0 + side)/2.0;
                    double y_mid = (y + y_end)/2.0 -  (1.0 + side)/2.0;

                    for(int i = 0; i < side; ++i){
                        for(int j = 0; j < side; j++){

                            coord_firm(side*i + j, 0) = i_firm;
                            coord_firm(side*i + j, 1) = 1 + j + x_mid;
                            coord_firm(side*i + j, 2) = 1 + i + y_mid;
                            coord_firm(side*i + j, 3) = level;

                            // add hierarchical power
                            if(level < maximum_rank){
                                coord_firm(side*i + j, 4) = hierarchal_power_vec[level - 1];
                            } else {
                                coord_firm(side*i + j, 4) = hierarchal_power_vec[maximum_rank - 1];
                            }

                        }
                    }

                    coord_all.insert_rows(coord_all.n_rows, coord_firm);

                }
            }
            ++ level;
        }

    }


    // individual pay a
    arma::vec pay_vec(coord_all.n_rows);

    double mu =  - 0.5*pow(sigma, 2);

    std::random_device rd;
    std::mt19937 gen(rd());
    std::lognormal_distribution<> d(mu, sigma);

    for(int i = 0; i < coord_all.n_rows; ++i){

        int firm_index = coord_all(i,0);
        int level = coord_all(i,3);
        double beta = beta_vec[firm_index];
        double base_pay = base_pay_vec[firm_index];

        double mean_power = coord_all(i,4);
        double mean_pay = base_pay*std::pow( mean_power, beta );

        pay_vec[i] = mean_pay*d(gen);

    }

    coord_all.insert_cols(coord_all.n_cols, pay_vec);

    return coord_all;

}


