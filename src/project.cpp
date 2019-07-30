#include <RcppArmadillo.h>

// projects 3d object onto 2d


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]


arma::mat project(  arma::mat a,
                    arma::vec c,
                    arma::vec theta,
                    arma::vec e
                    )
{   
    int n = a.n_rows;
  
    arma::mat output(n, 2);

    arma::mat m1;
    m1 << 1 << 0              << 0            << arma::endr
       << 0 << cos(theta[0])  << sin(theta[0]) << arma::endr
       << 0 << -sin(theta[0]) << cos(theta[0]) << arma::endr;


    arma::mat m2;
    m2 << cos(theta[1]) << 0   << -sin(theta[1])  << arma::endr
       << 0             << 1   << 0               << arma::endr
       << sin(theta[1]) << 0   << cos(theta[1])   << arma::endr;


    arma::mat m3;
    m3 << cos(theta[2])  << sin(theta[2])   << 0  << arma::endr
       << -sin(theta[2]) << cos(theta[2])   << 0  << arma::endr
       << 0              << 0               << 1  << arma::endr;


    arma::mat m_prod = m1*m2*m3;


    for(int i = 0; i < n; ++i){
        
        arma::vec v(3);
      
        v[0] = a(i, 0) - c[0];
        v[1] = a(i, 1) - c[1];
        v[2] = a(i, 2) - c[2];
        
        //arma::vec a_i = a.row(i);
        //arma::vec v = a_i - c;

        arma::vec d = m_prod*v;


        //double bx = e[2]/d[2]*d[0] - e[0];
        //double by = e[2]/d[2]*d[1] - e[1];

        output(i, 0) = e[2]/d[2]*d[0] - e[0];
        output(i, 1) = e[2]/d[2]*d[1] - e[1]; 


    }


    return output;

}






