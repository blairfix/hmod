// [[Rcpp::depends(BH)]]
// [[Rcpp::plugins(cpp11)]]

#include <Rcpp.h>
#include <boost/math/special_functions/zeta.hpp>
#include <random>
#include <algorithm>


using namespace std;
using namespace Rcpp;


// [[Rcpp::export]]

IntegerVector rpld(int n,
                   int xmin,
                   double alpha,
                   int discrete_max = 10000,
                   int xmax = 0,
                   bool ordered = false
                   )

{

    // output
    IntegerVector rng(n);


    // set upper limit of uniform dist
      double lim;
      if (xmax > xmin){
        lim =  1 -  pow( ( xmax - 0.5 )/( xmin - 0.5 ), 1 - alpha    ) ;
      } else{
        lim = 1;
      }


    // generate n random numbers from uniform distribution
    NumericVector u(n);
    u = runif(n, 0, lim);


    // check for low descrete_max
    if( discrete_max > 0.5 ){

        // define normalization constant
          double constant = boost::math::zeta(alpha);

          if (xmin > 1){
                for(int i = 1; i < xmin; i++ ){
                  constant = constant - pow(i, -alpha);
                }
          }


        // make cumulative distribution function up to discrete_max (CDF)
          NumericVector x_alpha(discrete_max);
          x_alpha[0] = (constant - pow(xmin, -alpha))/constant;

          NumericVector CDF (discrete_max + 1);
          CDF[1] =  1 - (constant -  pow(xmin, -alpha) ) /constant;


          for(int i = 1; i < discrete_max; i++){

              x_alpha[i] = x_alpha[i-1] -  pow(xmin + i, -alpha)/constant; // cumulative sum
              CDF[i+1] = 1 - x_alpha[i];

          }


        // sort u smallest to largest
          sort(u.begin(), u.end());


        ///////////////////////////////////////////////////////////////////////////////////////////////
        // discrete power law distribution
        // put u in correct CDF bin and get corresponding x value

          //int x = 1;
          int x = 1;

          // loop through sorted uniform distribution
          for(int i = 0; i <n; i++){

              if(x < discrete_max){

                  if( u[i] < CDF[x] ){                            // test if u below cdf and get corresponding x value

                        rng[i] = x + xmin - 1;

                  } else  {

                      while( u[i] >= CDF[x] && x < discrete_max ){  // advance x until CDF[x] < u[i]
                          x++;
                      }

                      if( u[i] < CDF[x]  ){
                        rng[i] = x + xmin - 1;
                      }

                  }
              }

          }

          // from end of rng,  search and replace zeros with continuous power law approximation
            int i = n-1;
            while(rng[i] == 0){

              rng[i] = floor( (xmin - 0.5) * pow(1 - u[i], -1/(alpha - 1) ) + 0.5);
              i--;

            }

      // shuffle if ordered = FALSE
      if(ordered == false){
        std::shuffle( rng.begin(), rng.end(), std::default_random_engine() );
      }


    } else {

        //////////////////////////////////////////////////////////////
        // generate rng using rounding method from continuous power law distribution
        for(int i = 0; i <n; i++){
          rng[i] =floor((xmin - 0.5) * pow(1 - u[i], -1/(alpha - 1)) + 0.5);
        }

        //order if ordered = TRUE
        if(ordered == true){
          sort(rng.begin(), rng.end());
        }


    }  // end discrete_max ifelse



  return rng;

  }


  // this code is based on Colin Gillespie's poweRlaw code for rpldis function, which is in turn based on Clauset's paper below
  // Clauset, Aaron, Cosma Rohilla Shalizi, and Mark EJ Newman. "Power-law distributions in empirical data." SIAM review 51.4 (2009): 661-703.


  // note: to enable c+11 compile, enter command below before compiling
  // Sys.setenv("PKG_CXXFLAGS"="-std=c++11 -O3 -march=native")







