# Complete data hybrid log-likelihood for Dirichlet SBM

hybrid_ll=function(Xd,Z,cl,a,theta){
  # Inputs: Xd - compositional data matrix (row sums=1) with zero diagonal removed, n-by-(n-1)
  #         Z - cluster allocations probabilities matrix, n-by-K
  #         cl - vector of cluster labels, 1-by-n
  #         a - vector version of Dirichlet parameters matrix Alpha, 1-by-(K^2)
  #         theta - vector of mixing proportions summing to 1, 1-by-K
  
  n=dim(Xd)[1] # Number of nodes, n
  d=dim(Xd)[2] # Dimensionality of Dirichlet observations, n-1
  K=dim(Z)[2] # Number of clusters, K
  Alpha=matrix(a,K,K,byrow=FALSE) # Original matrix of Dirichlet parameters
  
  # Compute contributions of each observation for each cluster, sum them up
  ll=0
  for (i in 1:n){
    for (k in 1:K){
      alpha_vec=Alpha[k,cl[-i]]
      log_pdf_dir=ddirichlet(Xd[i,],alpha_vec,log=TRUE)
      ll=ll+Z[i,k]*(log_pdf_dir+log(theta[k]))
    }
  }
  return(ll)
  # Outputs: ll - value of the complete data hybrid log-likelihood
}