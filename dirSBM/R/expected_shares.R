# Function to compute the expected total cluster-to-cluster exchange shares matrix V
# and the expected node-to-node exchange shares matrix W

expected_shares=function(Alpha,cls){
  # Inputs: Alpha - matrix of parameter estimates
  #         cls - vector of cluster labels
  
  K=dim(Alpha)[1]
  counts=matrix(NA,K,K)
  for (i in 1:K){
    counts[i,]=as.vector(table(cls))
  }
  diag(counts)=diag(counts)-1
  
  alpha_sums=rep(NA,K)
  for (i in 1:K){
    alpha_sums[i]=sum(counts[i,]*Alpha[i,])
  }
  
  W=matrix(NA,K,K)
  for (i in 1:K){
    W[i,]=Alpha[i,]/alpha_sums[i]
  }
  W=100*W
  V=counts*W
  outs=list(W=W,V=V)
  return(outs)
  # Outputs: W - K-by-K matrix of expected proportions sent between nodes (percentages)
  #          V - K-by-K matrix of expected total proportions sent between clusters (percentages)
}