// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;

//' Algorithm 1 for Online PCA
//'
//' @param x1 numeric vector
//' @param x2 numeric vector
//' @return dot product, that is \code{t(x1)%*%x2}
// [[Rcpp::export(algorithm1)]]
arma::field<arma::mat> algorithm1(arma::mat& x, double k, double eps)
{
  double x_frob = norm(x, "fro"); 
  double l = ceil((8 * k)/std::pow(eps,2));
  
  double d = x.n_cols;
  double n = x.n_rows;
  mat U(d,l);
  mat C(d,d);
  mat xr(d,n);
  
  int ind_u = -1;
  
  for (int i = 0; i < n; ++i) {
    
    Rcout << "i: " << i << std::endl;
    
    colvec r = x.row(i).t() - ((U * U.t())* x.row(i).t());
    
    while (norm(C + (r * r.t()),2) >= 2 * (pow(x_frob,2) / l)) {
      vec eigval;
      mat eigvec;
      
      eig_sym(eigval, eigvec, C);
      
      vec topEigvec = eigvec.col(eigval.index_max());
      double topEigval = eigval(eigval.index_max());
      
      ind_u = ind_u + 1;
      
      Rcout << "ind_u: " << ind_u << std::endl;
      
      U.col(ind_u) += topEigvec;
      
      C = C - (topEigval * (topEigvec * topEigvec.t()));
      r  = x.row(i).t() - ((U * U.t()) * x.row(i).t());
    }
    C = C + (r * r.t());
    xr.col(i) = U * U.t() * x.row(i).t();
  }
  
  arma::field<arma::mat> returnlist(2);
  returnlist(0) = xr;
  returnlist(1) = U;
  return returnlist;
}
