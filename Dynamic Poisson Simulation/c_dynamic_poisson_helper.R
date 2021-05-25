library(Rcpp)
library(RcppArmadillo)

sourceCpp("c_dynamic_poisson.cpp")

set.seed(90210)
DynamicPoisson(500)
