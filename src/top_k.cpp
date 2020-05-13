#include <RcppArmadillo.h>
#include <queue>
#include <vector>

// top_k finds the elements of data_vec associated with the 
// k largest elements in sort_vec.
// 
// inputs:
//     sort_vec = a vector of individual incomes
//     data_vec = a vector of firm sizes corresponding to each individual
//     k = the desired number of too incomes


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]


arma::rowvec top_k (const arma::vec &sort_vec,
                    const arma::vec &data_vec,
                    int k
                    )
{

    std::priority_queue< std::pair<double, int>, std::vector< std::pair<double, int> >, std::greater <std::pair<double, int> > > q;


    for (int i = 0; i < sort_vec.size(); ++i) {
        if(q.size() < k)
            q.push(std::pair<double, int>(sort_vec[i], i));
        else if( q.top().first < sort_vec[i] ){
            q.pop();
            q.push(std::pair<double, int>(sort_vec[i], i));
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
        output[i] = data_vec[res[i]];
    }


    return output;
}

