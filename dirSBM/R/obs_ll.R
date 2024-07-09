# Observed data hybrid log-likelihood of the Dirichlet SBM

obs_ll=function(Xd,cl,a,theta){
  # Inputs: Xd - compositional data matrix (row sums=1) with zero diagonal removed, n-by-(n-1)
  #         cl - cluster labels vector, 1-by-n
  #         a - vector version of Dirichlet parameters matrix Alpha, 1-by-(K^2)
  #         theta - vector of mixing proportions summing to 1, 1-by-K
  
  n=dim(Xd)[1] # Number of nodes, n
  d=dim(Xd)[2] # Dimensionality of Dirichlet observations, n-1
  K=sqrt(length(a)) # Number of clusters, K
  Alpha=matrix(a,K,K,byrow=FALSE) # Original matrix of Dirichlet parameters
  
  # Compute contributions of each observation for each cluster, sum them up
  ll=rep(0,n)
  for (i in 1:n){
    for (k in 1:K){
      alpha_vec=Alpha[k,cl[-i]]
      pdf_dir=ddirichlet(Xd[i,],alpha_vec,log=FALSE)
      ll[i]=ll[i]+pdf_dir*theta[k]
    }
  }
  ll=sum(log(ll))
  return(ll)
  # Outputs: ll - value of the observed data hybrid log-likelihood
}