# Model selection for Dirichlet SBM based on Integrated Completed Likelihood (ICL)

K_selection=function(X,K_max){
  # Inputs: X - compositional data matrix (row sums=1) with zero diagonal, n-by-n
  #         K_max - maximum number of clusters to consider
  
  n=dim(X)[1] # Number of nodes, n
  
  ICL=rep(NA,K_max)
  h_ll=rep(NA,K_max)
  runtime=rep(NA,K_max)
  
  Alphas=vector("list",K_max)
  Zs=vector("list",K_max) 
  cls=matrix(NA,nrow=K_max,ncol=n)
  
  for (k in 1:K_max){
    to_time=system.time({
      mod=dirSBM(X,k) # Fit the model with k=1,...,K_max clusters
      Alphas[[k]]=mod$Alpha # Store the Dirichlet parameter matrix estimates
      cls[k,]=mod$cl # Store the cluster labels
      Zs[[k]]=mod$Z # Store matrices with cluster assigment probabilities
      ICL_outs=ICL_dirSBM(X,mod$Z,mod$cl,as.vector(mod$Alpha),mod$theta)
      ICL[k]=ICL_outs$ICL # Store the model selection criterion value
      h_ll[k]=ICL_outs$h_ll # Store the value of complete data hybrid log-likelihood
    })
    runtime[k]=to_time[["elapsed"]] # Store time taken in seconds to fit the model with each k
    
    # Print progress
    print(paste0("Done fitting K=",k))
    print(paste0("Time taken in seconds=",runtime[k]))
    print(Sys.time())
  }
  outs=list(ICL=ICL,h_ll=h_ll,runtime=runtime,Alphas=Alphas,cls=cls,Zs=Zs) 
  return(outs)
  # Outputs: ICL - vector of Integrated Completed Likelihood (ICL) values for k=1,...,K_max
  #          h_ll - vector of values of complete data hybrid log-likelihood for k=1,...,K_max
  #          runtime - vector of running times in seconds, k=1,...,K_max
  #          Alphas - list of Dirichlet parameter matrix estimates, Alphas[[k]] is the estimate
  #                   of the model with k clusters
  #          cls - matrix of cluster labels, with k-th row containing solution with k clusters
  #          Zs - list of matrices of cluster allocation probabilities estimated at the E-step, 
  #               Zs[[k]] gives the probabilities from the model with k clusters
}
