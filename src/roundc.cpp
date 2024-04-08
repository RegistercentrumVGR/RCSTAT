#include <Rcpp.h>
using namespace Rcpp;

//' Rounding with ties-away-from-zero
//'
//' @param x numeric vector
//' @param digits number of digits to round to
//' @return rounded vector
//'
//' @export
// [[Rcpp::export]]
NumericVector roundc(const NumericVector x, const int digits = 0)
{
  const double multiplier = std::pow(10, digits);
  NumericVector y(x.size());
  std::transform(x.begin(), x.end(), y.begin(),
                 [multiplier](const double x) {
                   if (!R_FINITE(x)) return x;
                   return std::round(x * multiplier) / multiplier;
                 });
  return y;
}
