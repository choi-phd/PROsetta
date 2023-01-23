#ifndef _L_TO_EAP_H
#define _L_TO_EAP_H

// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

List LtoEAP_cpp(
  const arma::mat&,
  const arma::mat&,
  const arma::colvec&
);

#endif
