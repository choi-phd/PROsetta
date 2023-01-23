#include "LtoEAP.h"

//' @noRd
// [[Rcpp::export]]

List LtoEAP_cpp (
  const arma::mat& L,
  const arma::mat& theta_grid,
  const arma::colvec& prior_density
) {

  int n_score = L.n_cols;
  int nq = theta_grid.n_rows;
  int nd = theta_grid.n_cols;

  List o(n_score);

  for (int s = 0; s < n_score; s++) {

    // common
    double denom = sum(L.col(s) % prior_density);

    // EAP
    arma::rowvec num = (theta_grid.t() * (L.col(s) % prior_density)).t();
    arma::rowvec EAP = num / denom;

    arma::mat diff_grid = theta_grid.each_row() - EAP;

    List term_V(nq);

    for (int q = 0; q < nq; q++) {
      term_V[q] = diff_grid.row(q).t() * diff_grid.row(q);
    }

    arma::mat num_COV(nd, nd);

    for (int q = 0; q < nq; q++) {
      double w = L(q, s) * prior_density(q);
      arma::mat thismat = term_V[q];
      num_COV += thismat * w;
    }

    o[s] = List::create(
      Named("EAP") = EAP,
      Named("COV") = num_COV / denom
    );

  }

  return o;

}
