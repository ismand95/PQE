# definition of the gradient of the 2D Rosenbrock function (not defined as function)
fGrad <- function(v) {
  c(-400 * v[1] * (v[2] - v[1]*v[1]) - 2 * (1 - v[1]), 200 * (v[2] - v[1]*v[1]))
}

vX = c(0,0)
dEps = 1e-9

iT = 0
iN = length(vX)
mM = diag(iN)

fprimeX = fGrad(vX)  

while( max(abs(fprimeX)) > dEps ){
  vS = solve(-mM, fprimeX)
  vX = vX + vS
  cat(vX,"\n")
  
  fprimeX1 = fGrad(vX)
  vQ = fprimeX1 - fprimeX
  
  mM = mM + (vQ %*% t(vQ))/(t(vQ) %*% vS)[1] - (mM %*% vS %*% t(vS) %*% t(mM))/ (t(vS) %*% mM %*% vS)[1]
  fprimeX = fGrad(vX)  
  cat("fprime = ",fprimeX,"\n")
  
}



