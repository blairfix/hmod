#include <Rcpp.h>
#include <Ziggurat.h>
using namespace std;
using namespace Rcpp;

static Ziggurat::Ziggurat::Ziggurat zigg;

// [[Rcpp::depends(RcppZiggurat)]]
// [[Rcpp::export]]



NumericMatrix model(    double a,
                        double b,
                        NumericVector base_emp_vec,
                        IntegerVector emp_vec,
                        NumericVector base_pay_vec,
                        NumericVector r_vec,
                        double sigma,
                        bool firm_size = false,
                        bool hierarchy = false,
                        bool power = false
                        )

{

    //////////////////////////////////////////////////////////////////////////////////////////////
    // mandatory output
         int n_cols = 1; // keep track of number of columns in output matrix
         int pay_col = 0;       int p_index = 0;

    // optional outputs
        int firm_size_col;  int firm_size_index = 0;
        int hier_col;       int hier_index = 0;
        int power_col;      int power_index = 0;

        StringVector names(4);
        names[0] = "pay";


          // firm size export
          if( firm_size == true ){
            firm_size_col = n_cols;
            names[n_cols] = "firm_size";
            n_cols++;
          }

          // hierarchical levels export
          if( hierarchy == true ){
            hier_col = n_cols;
            names[n_cols] = "h_level";
            n_cols++;
          }


          // power export
          if( power == true ){
            power_col = n_cols;
            names[n_cols] = "power";
            n_cols++;
          }

          if(n_cols < 4){ names.erase(n_cols,4);   }


  // output matrix
    NumericMatrix output( sum(emp_vec), n_cols);
    colnames(output) = names;

//  int nrows = sum(emp_vec);
 // NumericMatrix output( nrows, 4);


  ////////////////////////////////////////////////////////////////////////////////////////////////
  // begin model


  // intermediate variables
  int   n_firm = base_emp_vec.size();
  double base;
  int check;
  int emp;
  double base_pay;
  double mu = log(1) - 0.5*pow(sigma, 2);
  double r;

    // calculate exponents, span of control ... things that are the same for all firms

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


  ////////////////////////////////////////////////////////////////////////////////
  // loop over firms
      for(int firm_index = 0; firm_index < n_firm; firm_index++){

          base = base_emp_vec[firm_index];
          emp = emp_vec[firm_index];
          r = r_vec[firm_index];
          base_pay = base_pay_vec[firm_index];


          // employment model

              // hierarchical employment vector
                NumericVector h(20);

              // set base level
                h[0] =  floor(base);

              // max = maximum hierarchical level
                int max;

              // correct to allow firm size of 1
               if( h[0] < 1 ){ h[0] = 1 ;}

                for (int i = 1; i < 20; i++){

                  h[i] =  floor(   base / sprod[i-1]  ) ;
                  // test if level i is first  0 value, if yes, i is max
                  if(h[i] == 0 &&  h[i-1] > 0 ) { max = i; };

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


          // pay model

              double p;  // pay

              //loop over levels
              for (int i = 0; i < max; i++){
                p = pow( r, e_pay[i]);
                int n = (int)h[i];

                // loop over individuals
                for (int j = 0; j < n; j++){

                  output(p_index, pay_col) = base_pay*p*exp( sigma*zigg.norm() + mu );
                  p_index++;

                }
              } // end loop over levels

          //////////////////////////////////////////////////////////////////////////////
          // optional calculations

              // firm size
                if( firm_size == true){
                  for(int i =0; i < emp; i++){
                    output(firm_size_index, firm_size_col) = emp;
                    firm_size_index++;
                  }
                }


              // hierarchical levels
                if(hierarchy == true){
                      //loop over levels
                      for (int i = 0; i < max; i++){

                        int n = (int)h[i];
                        // loop over individuals
                        for (int j = 0; j < n; j++){
                          output(hier_index, hier_col) = i+1;
                          hier_index++;
                        }
                      } // end loop over levels
                } // end if


             // power
                if(power == true){

                  double n_subord = 0;

                    //loop over levels
                    for (int i = 0; i < max; i++){

                      int n = (int)h[i];
                      double power_mean = n_subord/n + 1;

                      // loop over individuals
                      for (int j = 0; j < n; j++){
                        output(power_index, power_col) = power_mean;
                        power_index++;
                      }

                    n_subord = n_subord + h[i];

                  } // end loop over levels
                } // end if


      } // end loop over firms


  return output;
}

