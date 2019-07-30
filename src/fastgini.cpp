#include <Rcpp.h>
using namespace std;
using namespace Rcpp;

// [[Rcpp::export]]
double fastgini(NumericVector x, bool corr = false)
  {
  int n = x.size();


  sort(x.begin(), x.end());
  double G = 0;

  for(int i = 0; i < n; i++){
    G = G + x[i]*(i+1);
  }

  G = 2*G/sum(x) - (n + 1);

    if(corr){
      G = G/(n-1);
    } else {
      G = G/n;
    }


  if(n == 1){G = 0;}

  return G;
  }

