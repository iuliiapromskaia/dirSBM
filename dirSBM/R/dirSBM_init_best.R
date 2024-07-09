# Additional function for random initialisation: reinitialise "runs" times and select the best run

dirSBM_init_best=function(X,K,init=c("random"),runs){
  # Inputs: X - compositional data matrix (row sums=1) with zero diagonal present, n-by-n
  #         K - number of clusters to be tried
  #         init - initialisation strategy for clustering allocation (random)
  #         runs - number of random starting partitions
  
  outs=foreach(r=1:runs,.export=ls(envir=globalenv()),.packages = c("LaplacesDemon","e1071")) %dopar% {
    n=dim(X)[1]
    model=try(dirSBM_init(X,K,init),TRUE)
    while (is.character(model)==TRUE){
      model=try(dirSBM_init(X,K,init),TRUE)
    }
    return(model)
  }
  
  ll_runs=rep(NA,runs)
  for (i in 1:runs){
    ll_runs[i]=tail(outs[[i]]$llvals,n=1) # Observed hybrid log-likelihood values at convergence
  }
  # Select the best run based on observed hybrid log-likelihood value
  best=which.max(ll_runs)
  llvals=outs[[best]]$llvals
  Alpha=outs[[best]]$Alpha # Dirichlet parameter matrix estimator of the best run
  cl=outs[[best]]$cl # Cluster labels of the best run
  theta=outs[[best]]$theta # Mixing proportions of the best run
  Z_hard=outs[[best]]$Z_hard # Matrix of cluster allocations of the best run
  Z=outs[[best]]$Z # Matrix of cluster allocations of the best run
  res=list(cl=cl,Z_hard=Z_hard,Z=Z,theta=theta,Alpha=Alpha,llvals=llvals)
  return(res)
  # Outputs: cl - vector of cluster labels of the best run
  #          Z_hard - binary cluster allocations matrix of the best run
  #          Z - cluster allocation probability matrix of the best run
  #          theta - mixing proportions
  #          Alpha - Dirichlet parameters matrix estimate of the best run
  #          llvals - values of observed hybrid log-likelihood
}
