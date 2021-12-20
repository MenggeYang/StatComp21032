#include <Rcpp.h>
using namespace Rcpp;

//' @title A Gibbs sampler using Rcpp
//' @description A Gibbs sampler using Rcpp
//' @param n binomial distribution coefficient
//' @param a the beta distribution with parameters shape1
//' @param b the beta distribution with parameters shape2
//' @param N the shape size
//' @examples
//' dontrun{
//' n = 20
//' a = 1
//' b = 2
//' N = 2000
//' method_1(n,a,b,N)
//' }
//' @export
NumericMatrix method_1(int n, int a, int b, int N){
  NumericMatrix shuchu(N, 2);
  double x1=1;
  double x2=0.66;
  double canshu1=1;
  double canshu2=1;
  for(int i=0; i < N; i++){
    x1 = rbinom(1, n, x2)[0];
    canshu1 = x1 + a;
    canshu2 = n - x1 + b;
    x2 = rbeta(1, canshu1, canshu2)[0];
    shuchu(i,0) = x1;
    shuchu(i,1) = x2;
  }
  return(shuchu);
}
