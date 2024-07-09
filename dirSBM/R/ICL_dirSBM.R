# Integrated Completed Likelihood (ICL) of Dirichlet SBM

ICL_dirSBM=function(X,Z,cl,a,theta){ 
  # Inputs: X - compositional data matrix (row sums=1) with zero diagonal, n-by-n
  #         Z - matrix of cluster allocation probabilities, n-by-K
  #         cl - vector of cluster labels, 1-by-n
  #         a - vector version of Dirichlet parameters matrix Alpha, 1-by-(K^2)
  #         theta - vector of mixing proportions summing to 1, 1-by-K
  
  # Remove zero diagonal from X
  Xd=matrix(NA,n,n-1)
  for (i in 1:dim(X)[1]){
    Xd[i,]=X[i,-i]
  }
  
  n=dim(Xd)[1] # Number of nodes, n
  d=dim(Xd)[2] # Dimensionality of Dirichlet observations, n-1
  K=dim(Z)[2] # Number of clusters, K
  
  h_ll=hybrid_ll(Xd,Z,cl,a,theta) # Complete data hybrid log-likelihood value
  ICL=h_ll-0.5*K^2*log(n*(n-1))-0.5*(K-1)*log(n) # ICL value
  outs=list(h_ll=h_ll,ICL=ICL)
  return(outs)
  # Outputs: h_ll - complete data hybrid log-likelihood value
  #          ICL - Integrated Completed Likelihood (ICL) value
}
