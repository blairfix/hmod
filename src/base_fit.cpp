#include <RcppArmadillo.h>
using namespace std;
using namespace Rcpp;


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]


    
NumericVector base_fit( double a, 
                        double b, 
                        NumericVector emp_vec) 
  {
    
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    // initialize and fill trial base vector 
      int n_trial = 10000; // length of base_trial
      int linear = 2000; // number of linear steps in base_trial
      
      NumericVector base_trial(n_trial);
      base_trial[0] = 0.02;
      
          // fill linear steps in base_trial
          for (int i = 1; i < linear; i++){
              base_trial[i] = base_trial[i-1] + 0.05;
          }
          
          // check max of emp vec to inform max of base_trial vector
          double max  =* max_element( emp_vec.begin(), emp_vec.end() );
          if( max < 2*pow(10, 6) ){ max = 2*pow(10, 6); } // max not below 2 million
            
          // get r such that base_trial[linear-1]*r^(l-linear) = max
          double r = exp( (log(max) -  log(base_trial[linear-1])  )/ (n_trial -linear)  ) ;
  
          // fill the rest of base_trial with exponential sequence
          for (int i = linear; i < n_trial; i++){
            base_trial[i] = base_trial[i-1]*r;
          }
        
     
    ///////////////////////////////////////////////////////////////////////////////////////////////////////
    // employment model
        
        // initialize and fill exponent vector e_emp
            IntegerVector e_emp(20);
            for (int i = 0; i < 20; i++){
              e_emp[i] = i+2;
            }
      
          // span of control vector
            NumericVector s(20);
      
            // fill span vector with values s = a*e^b
            for (int i = 0; i < 20; i++){
              s[i] = a*exp(b*e_emp[i]);
            }
        
          // cumulative product of span vector
            NumericVector sprod = cumprod(s);
        
        
        ////////////////////////////////////////////////////////////
        // loop over base_trial and get employment for each base
            NumericVector emp_trial(n_trial);
            
              for(int b_index =0; b_index < n_trial; b_index++){

                      // base employment
                      double base = base_trial[b_index];
              
                      // hierarchical employment vector
                      IntegerVector h(20);
    
                      // set base level
                      h[0] = (int) floor(base);
                      
                      // correct to allow firm size of 1
                      if( h[0] < 1  ){ h[0] = 1 ;}  
                      
                      // fill hierarchical levels
                      for (int i = 1; i < 20; i++){
                        h[i] = (int)  floor(   base / sprod[i-1]  ) ;
                      }
              
                      emp_trial[b_index] = sum(h);
                
                } // end base vector loop
    
    
    ///////////////////////////////////////////////////////////////////////////////////////
    // find unique, non-zero elements of emp_trial and match with corresponding base level
    
        NumericVector emp_unique(n_trial);
        NumericVector base_unique(n_trial);
        
        emp_unique[0] = 1;
        base_unique[0] = base_trial[0];
        int unique_index = 1;
    
        for(int i = 1; i < n_trial; i++){   
            if(emp_trial[i] > emp_trial[i-1]){ 
              emp_unique[unique_index] = emp_trial[i];
              base_unique[unique_index] = base_trial[i];
              unique_index++;
            }
        }
    
    
      // model base and total employent vectors used for regression:
        NumericVector emp_model = emp_unique[emp_unique >=1];
        NumericVector base_model = base_unique[emp_unique >=1];
        

      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      // REGRESSIONS  
        
        // start & stop = emp_model indexes for 3 different poly regression 
        // (note overlap to avoid holes)
        NumericVector start_vec = NumericVector::create(0, 19, 999 );
        NumericVector stop_vec = NumericVector::create(20, 1000, emp_model.size() );
        IntegerVector degree = IntegerVector::create(5, 10, 15); // degree of each poly

        
        // initialize coefficiant matrix beta ()
        // rows = each polynomial 
        // cols = coefficients of degree = col#
        NumericMatrix beta_matrix(degree.size(),  degree[degree.size() -1] + 1   );
        
        // loop over polynomials
        for(int poly_index = 0; poly_index < degree.size(); poly_index++){

              int start = start_vec[poly_index]; 
              int stop = stop_vec[poly_index]; 
              
              int nrow =  stop - start;           // number of rows in design matrix
              int ncol = degree[poly_index] + 1;  // number of cols in design matrix
              
              NumericMatrix X(nrow, ncol);        // X = design matrix
              NumericVector y(nrow);              // y = response vector
              
              // fill design matrix X
              // i = column, j = row
              for(int i = 0; i < nrow; i++){
                for(int j = 0; j < ncol; j++){
                  
                  X(i, j) = pow( emp_model[ start + i]  , j );
                  y[i] =  base_model[ start + i ] ;
                }
              }
              
              // convert X & y to arma for matrix transformation
              arma::mat X_arma = as<arma::mat>(X);
              arma::vec y_arma = as<arma::vec>(y);  

              // get coefficients (beta) of poly regression
              // see wikipedia.org/wiki/Polynomial_regression for the math
              arma::vec beta = inv(  X_arma.t()*X_arma )*X_arma.t()*y_arma;
              
              // convert beta to Rcpp NumericVector
              NumericVector beta_rcpp = wrap(beta); 
              
              // fill beta_matrix
                for(int i = 0; i < beta.size(); i++ ){
                  beta_matrix(poly_index, i) = beta_rcpp[i];
                }

        } // end poly loop

        
        ////////////////////////////////////////////////////////////////////////////////////////////////////
        // Use polynomial to predict base from emp_vec
        
          NumericVector base_predict_vec(emp_vec.size());

          ///////////////////////////////////////////
          // loop over firms
          for(int i = 0; i < emp_vec.size() ; i++){
            
  
            // test which polynomial to use for given firm (i)
              int poly_index = -1;
              bool search = false;
              
              while( search == false){
                poly_index++;  
                int upper = stop_vec[poly_index] - 1;
                if( emp_vec[i] <= emp_model[ upper ] ){
                  search = true;
                }
              }
            
              int ncol = degree[poly_index] + 1;  // number of cols in appropriate beta.matrix row
              double poly_sum = 0;                // running sum of polynomial terms  
  
              
              // loop over poly terms and sum to get predicted base
              for(int j = 0; j < ncol; j++){
                poly_sum = poly_sum + beta_matrix(poly_index, j) * pow( emp_vec[i] , j);
              }
  
              base_predict_vec[i] = poly_sum;
  
           } // end loop over firms
 
        
  return base_predict_vec;
}

