#include <Rcpp.h>
using namespace std;
using namespace Rcpp;


// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]

double   top ( NumericVector pay, 
                      double frac
                    ) 
{

  int n = pay.size();
  int k = frac*n;
  
  
  double total = sum(pay);

  nth_element(pay.begin(), pay.end() - k, pay.end());
  

  double top = 0;
  
  for(int i = n- k; i < n; i++){
    top = top + pay[i];
  }

  return top/total ;
} 

