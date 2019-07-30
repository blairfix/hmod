#include <Rcpp.h>
#include <queue>
#include <vector>
using namespace std;
using namespace Rcpp;


// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]

IntegerVector top_k ( NumericVector pay,
                      IntegerVector emp,
                      int k
                    )
{


  std::priority_queue< std::pair<double, int>, std::vector< std::pair<double, int> >, std::greater <std::pair<double, int> > > q;


  for (int i = 0; i < pay.size(); ++i) {
    if(q.size()<k)
      q.push(std::pair<double, int>(pay[i], i));
    else if(q.top().first < pay[i]){
      q.pop();
      q.push(std::pair<double, int>(pay[i], i));
    }
  }
  k = q.size();
  IntegerVector res(k);

  for (int i = 0; i < k; ++i) {
    res[k - i - 1] = q.top().second;
    q.pop();
  }


  return emp[res];
}

