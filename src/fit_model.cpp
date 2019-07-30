#include <Rcpp.h>
using namespace std;
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]



NumericMatrix fit_model(double a,
                        double b,
                        NumericVector base_emp_vec,
                        IntegerVector emp_vec,
                        NumericVector c_r_vec,
                        NumericVector m_pay_vec,
                        double tol
                        )

  {

  // intermediate variables
      int   n_firm = base_emp_vec.size();
      double base;
      double best_error;
      double c_r;
      int emp;
      NumericVector error(3);
      NumericVector m_pay_model_vec(n_firm)   ;
      double m_pay_model;
      double predict;
      NumericVector r(3);
      int while_counter;

  // NumericVector test(emp_vec.size());


  // export variables
      NumericVector predict_vector(n_firm);
      NumericVector error_vector(n_firm);
      NumericVector base_pay_vector(n_firm);

  // output matrix
      NumericMatrix output(n_firm, 3)  ;
      colnames(output) = CharacterVector::create("r", "ceo_error", "base_pay");

  // exponent vectors

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


            //  initialize and fill exponent vector e_pay
            IntegerVector e_pay(20);

            for (int i = 0; i < 20; i++){
              e_pay[i] = i;
            }

            // cumulative sum of e
            partial_sum(e_pay.begin(), e_pay.end(), e_pay.begin());


  /////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // loop over firms

      for(int firm_index = 0; firm_index < n_firm; firm_index++){

          base = base_emp_vec[firm_index];
          c_r = c_r_vec[firm_index];
          emp = emp_vec[firm_index];

          // employment model

              // hierarchical employment vector
              NumericVector h(20);

              // set base level
              h(0) = floor(base);

              // correct to allow firm size of 1
              if( h[0] < 1 ){ h[0] = 1 ;}

              // max = maximum hierarchical level
              int max = 0;

              // loop over levels 2 and greater (level 2 is j=1)
              for (int j = 1; j < 20; j++){

                h[j] =  floor(   base / sprod[j-1]  ) ;
                // test if level j is first  0 value, if yes, j is max
                if(h[j] == 0 &&  h[j-1] > 0 ) { max = j; };

              }



              // check for employment over/undershoot
              int check = sum(h) - emp;

              // if h[0] - check <= 0 subtract overshoot from top levels
              // otherwise add/subtract to bottom level
              if( h[0] - check <= 0 ) {
                while (check > 0){
                  h[max - 1] = h[max - 1] - 1;
                  check =  check -1;
                  if(h[max - 1] == 0 ){ max = max - 1; }
                }
              }
              else {
                h[0] = h[0] - check;
              }



          //////////////////////////////////////////////////////////////////////////////////////////////////
          // pay scaling (r) optimization using bisection method
          best_error = 100;
          while_counter = 1;

          // r0 = left bound
          // r1 = right bound
          // r2 =  test value

          r[0] = 1.0;
          r[1] = 1.7;
          //r[2] = 1.35;
          int d_start;
          int d_stop;

                // while loop until error under tolerance or max iterations exceeded
                while (best_error > tol && while_counter < 50 ){

                  // test if first loop
                  if(while_counter == 1){
                      // yes ... loop over r0 and r1
                      d_start = 0;
                      d_stop = 2;
                  } else {
                      // no ...  loop over r2 only
                      d_start = 2;
                      d_stop = 3;
                  }


                    // loop over r to get error
                    for (int d_index = d_start; d_index < d_stop; d_index++){

                          // pay model

                                // pay vector
                                NumericVector p(max);

                                // loop over levels
                                for (int j = 0; j < max; j++){
                                      p[j] = pow( r[d_index], e_pay[j]) ;
                                    }


                            // calculate firm mean pay (weighted mean of p by h vectors)
                                double total = 0, total_w = 0;
                                for(int i = 0; i < max; ++i){

                                  total += p[i] * h[i];
                                  total_w += h[i];

                                }

                                        m_pay_model =  total / total_w;
                                double  top_exec = p[max-1];
                                double  ratio = top_exec/m_pay_model;

                        error[d_index] =  c_r - ratio;

                    } // end r loop

                // bisector calculations
                // if not first loop ... get new bounds
                if (while_counter > 1){

                    // check if left bound and test value are opposite signs
                    if( error[0]*error[2] < 0.0 ){
                      // yes ... r2 is new right bound
                      r[1] = r[2];
                      error[1] = error[2];
                    }
                    else  {
                      // no ... r2 is new left bound
                      r[0] = r[2];
                      error[0] = error[2];
                    }

                } // end if

                // get bisector of left and right bounds
                r[2] = (r[0] +r[1])/2;

                // best error is error at midpoint
                best_error = abs(error[1]) ;
                while_counter = while_counter + 1;

             } // end error while loop

        // fill results vectors
        m_pay_model_vec[firm_index] = m_pay_model;
        predict_vector[firm_index] = r[1];
        error_vector[firm_index] = best_error;


      }// end loop over firms
  /////////////////////////////////////////////////////////////////////////////////////////////

  // get Compustat base pay
  base_pay_vector = m_pay_vec/ m_pay_model_vec;

  // fill output matrix
  output( _ , 0) = predict_vector;
  output( _ , 1) = error_vector;
  output( _ , 2) = base_pay_vector;

  return output;
}

