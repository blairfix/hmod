#include <RcppArmadillo.h>
#include <algorithm>
#include <random>
#include <math.h>



// firm_grid turns firms into pyramids and places
// them on a grid.

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(BH)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]


arma::imat firm_grid(arma::vec base_vec, int stretch)
{

    arma::imat firm_coord(base_vec.size(), 5);
    int n = stretch*sum(base_vec);
    int side = ceil(sqrt(n));

    arma::umat coord(side, side);
    coord.fill(1);


    // grid consecutive lines
    arma::umat grid_box(0,2);

    for(int i = 0; i < side; ++i){

        arma::umat col(side,2);
        col.col(0).fill(i);
        col.col(1) = arma::regspace<arma::uvec>(0, side - 1);
        grid_box.insert_rows(grid_box.n_rows, col);

    }



    int n_firm = base_vec.size();
    int firm_index = 0;
    int grid_index = 0;
    bool more_firms = true;

    while(more_firms){

        bool occupied = true;
        while(occupied){

            int x = grid_box(grid_index, 0);
            int y = grid_box(grid_index, 1);
            int shadow = round(sqrt(base_vec[firm_index])) + 2;
            int firm_side = round(sqrt(base_vec[firm_index])) ;

            if( x + shadow > side-1 | y + shadow > side -1){
                ++grid_index;
            } else {

                occupied = arma::any( vectorise(  coord( arma::span(x, x + shadow), arma::span(y, y + shadow) ) ) == 0);

                if(occupied){

                    if (grid_index % 10 == 0){ Rcpp::checkUserInterrupt();  }
                    ++grid_index;

                } else {

                    int x_shift = x + 1;
                    int y_shift = y + 1;

                    coord( arma::span(x_shift, x_shift + firm_side), arma::span(y_shift, y_shift + firm_side))  = arma::zeros<arma::umat>(firm_side + 1, firm_side +1);

                    firm_coord(firm_index,0) = firm_index;
                    firm_coord(firm_index, 1) = x_shift;
                    firm_coord(firm_index, 2) = x_shift + firm_side;
                    firm_coord(firm_index, 3) = y_shift;
                    firm_coord(firm_index, 4) = y_shift + firm_side;

                    ++firm_index;
                    if(firm_index == n_firm){ more_firms = false; }
                }
            }
        }
    }

    return firm_coord;

}


