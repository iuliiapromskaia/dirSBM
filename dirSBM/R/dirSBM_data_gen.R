# Data generation for Dirichlet SBM

dirSBM_data_gen=function(n,K,Alpha){
  # Inputs: n - number of nodes/observations
  #         K - number of clusters
  #         Alpha - Dirichlet parameter matrix
  
  ### Generate cluster memberships
  clusters=sample(1:K,n,rep(1/K,K),replace=TRUE)
  Z=matrix(NA,nrow=n,ncol=K)
  for (k in 1:K){
    Z[,k]=ifelse(clusters==k,1,0)
  }
  
  ### Note: the following way is equivalent to generating Dirichlet observations directly
  # Generate independent Gamma observations
  X=matrix(0,n,n)
  for (i in 1:n){
    for (j in 1:n){
      X[i,j]=rgamma(1,Alpha[clusters[i],clusters[j]],1)
    }
  }
  diag(X)=0
  
  # Convert data from independent Gammas to Dirichlet observations, i.e. compositions
  for (i in 1:n){
    if (sum(X[i,]>0)){
      X[i,]=X[i,]/sum(X[i,])
    }
  }
  
  # Create network
  net=graph_from_adjacency_matrix(X,mode="directed",weighted=TRUE)
  E(net)$width=E(net)$weight
  
  clusters_true=clusters
  Z_true=Z
  
  outputs=list(X=X,net=net,clusters_true=clusters_true,Z_true=Z_true)
  return(outputs)
  # Outputs: X - compositional data matrix (row sums=1) with zero diagonal
  #          net - directed weighted network object
  #          clusters_true - true cluster labels used to generate data
  #          Z_true - true binary cluster allocations matrix
}
