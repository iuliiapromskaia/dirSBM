# Classification EM algorithm with hybrid log-likelihood for Dirichlet SBM
# with fixed number of clusters and random initialisation for cluster labels

dirSBM=function(X,K){
  # Inputs: X - compositional data matrix (row sums=1) with zero diagonal, n-by-n
  #         K - number of clusters to find
  
  init="random" # Initialised with random partition
  runs=5 # Number of random partitions for initialisation to try
  model=dirSBM_init_best(X,K,init,runs)
  Alpha=model$Alpha # Dirichlet parameter matrix estimator of the best run
  cl=model$cl # Cluster labels of the best run
  Z_hard=model$Z_hard # Matrix of cluster allocations of the best run
  Z=model$Z # Matrix of cluster allocations of the best run
  theta=model$theta # Vector of mixing proportions
  ll=tail(model$llvals,n=1)
  res=list(Z_hard=Z_hard,Z=Z,cl=cl,theta=theta,Alpha=Alpha,ll=ll)
  return(res)
  # Outputs: Z_hard - binary matrix of cluster allocations
  #          Z - cluster allocation probabilities matrix
  #          cl - vector of cluster labels
  #          theta - mixing proportions
  #          Alpha - Dirichlet parameter matrix estimator
  #          ll - value of observed hybrid log-likelihood value 
}
