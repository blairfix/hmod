#include <RcppArmadillo.h>
#include <queue>
#include <vector>


// top_k finds the firm sizes associated with the k individuals with largest incomes.
// inputs:
//     pay = a vector of individual incomes
//     emp = a vector of firm sizes corresponding to each individual
//     k = the desired number of too incomes



// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]


arma::rowvec top_k (   const arma::vec &pay,
                        const arma::vec &emp,
                        int k
                    )
{

    std::priority_queue< std::pair<double, int>, std::vector< std::pair<double, int> >, std::greater <std::pair<double, int> > > q;


    for (int i = 0; i < pay.size(); ++i) {
        if(q.size() < k)
            q.push(std::pair<double, int>(pay[i], i));
        else if( q.top().first < pay[i] ){
            q.pop();
            q.push(std::pair<double, int>(pay[i], i));
        }
    }

    k = q.size();
    arma::uvec res(k);

    for (int i = 0; i < k; ++i) {
        res[k - i - 1] = q.top().second;
        q.pop();
    }


    arma::rowvec output(k);

    for(int i = 0; i < k; i++){
        output[i] = emp[res[i]];
    }


    return output;
}

