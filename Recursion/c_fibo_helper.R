library(Rcpp)
library(RcppArmadillo)

sourceCpp("c_fibo.cpp")

Fibonacci(10)

# into vector
sapply(seq(1, 20), FUN = Fibonacci)