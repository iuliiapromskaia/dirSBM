# Classification EM algorithm for the Dirichlet SBM with a choice of initialisation strategy

dirSBM_init=function(X,K,init=c("random","kmeans","clr_kmeans","specc","sbm_bin","sbm_gaus")){
  # Inputs: X - compositional data matrix (row sums=1) with zero diagonal present, n-by-n
  #         K - number of clusters to be tried
  #         init - initialisation strategy for clustering allocation (random, k-means,
  #                k-means on centered log-ratio transformed data, spectral clustering, binary SBM, Gaussian SBM)
  
  # Remove zero diagonal from data matrix X
  
  n=dim(X)[1] # Number of nodes, n
  
  Xd=matrix(NA,n,n-1)
  for (i in 1:dim(X)[1]){
    Xd[i,]=X[i,-i]
  }
  
  d=dim(Xd)[2] # Dimensionality of Dirichlet observations, n-1
  
  ##### Functions that fit the binary SBM and Gaussian SBM for initialisation
  # Binary SMB
  binary_sbm=function(X,K){
    adj=ifelse(X>mean(X),1,0)
    sbm=BM_bernoulli(membership_type="SBM",adj=adj,verbosity=0,plotting="",explore_min=K,explore_max=K)
    sbm$estimate()
    clusters_sbm=apply(sbm$memberships[[K]]$Z,1,which.max)
    return(clusters_sbm)
  }
  
  # Gaussian SBM on compositional data directly (naive)
  gaus_sbm_naive=function(X,K){
    adj=as.matrix(X)
    sbm=BM_gaussian(membership_type="SBM",adj=adj,verbosity=0,plotting="",explore_min=K,explore_max=K)
    sbm$estimate()
    clusters_g_sbm_naive=apply(sbm$memberships[[K]]$Z,1,which.max)
    return(clusters_g_sbm_naive)
  }
  #####
  
  
  ##############################
  # Initialise clustering allocations based on the choice of initialisation strategy
  if (init=="random") {
    cl=sample(1:K,n,replace=TRUE)
  }
  if (init=="kmeans") {
    cl=kmeans(Xd,K,nstart=50)$cluster
  }
  if (init=="clr_kmeans") {
    cl=kmeans(clr(Xd),K,nstart=50)$cluster
  }
  if (init=="specc") {
    cl=as.vector(specc(Xd,K))
  }
  if (init=="sbm_bin") {
    cl=binary_sbm(X,K)
  }
  if (init=="sbm_gaus") {
    cl=gaus_sbm_naive(X,K)
  }
  #cl_mat=cl # If want to store cluster labels at each iteration
  
  Z_hard=matrix(NA,nrow=n,ncol=K)
  for (k in 1:K){
    Z_hard[,k]=ifelse(cl==k,1,0)
  }
  
  theta=colSums(Z_hard)/n
  par_init=abs(rnorm(K^2,1,0.25))
  opt=optim(par=par_init,lower=rep(0.01,length(par_init)),fn=hybrid_ll,Xd=Xd,Z=Z_hard,cl=cl,
            theta=theta,method="L-BFGS-B",control=list(fnscale=-1)) # Numerical optimisation for parameter matrix Alpha
  a=opt$par
  Alpha=matrix(a,K,K,byrow=FALSE)
  #Alpha_list=vector("list") # If want to store Alpha estimates at each iteration of the algorithm
  #Alpha_list=c(Alpha_list,list(Alpha))
  
  # Initialisation complete
  
  llvals=ll_new=c(obs_ll(Xd,cl,a,theta)) # Observed hybrid log-likelihood values
  ll_old=0.5*ll_new
  #c_llvals=c(opt$value) # Complete data hybrid log-likelihood values
  
  while(abs(((ll_new-ll_old)/ll_new))>10^(-5)){ 
    ##############################
    # E-step
    Z=matrix(NA,n,K)
    ll=rep(NA,n)
    for (i in 1:n){
      for (k in 1:K){
        alpha_vec=Alpha[k,cl[-i]]
        log_pdf_dir=ddirichlet(Xd[i,],alpha_vec,log=TRUE)
        Z[i,k]=log(theta[k])+log_pdf_dir
      }
      m=max(Z[i,])
      ll[i]=m+log(sum(exp(Z[i,]-m))) # Log-sum_exp trick to normalise probabilities
      Z[i,]=exp(Z[i,]-ll[i])
    }
    
    ##############################
    # Greedy classification step
    for (i in 1:n){
      ll=rep(NA,K)
      for (k in 1:K){
        cl[i]=k # Assign node i to each cluster in turn
        ll[k]=obs_ll(Xd,cl,a,theta) # Compute observed hybrid log-likelihood value with i in cluster k
      }
      cl[i]=which.max(ll) # Assign node i to cluster with highest observed hybrid log-likelihood 
    }
    #cl_mat=rbind(cl_mat,cl) # If want to store cluster labels at each iteration
    Z_hard=matrix(NA,nrow=n,ncol=K)
    for (k in 1:K){
      Z_hard[,k]=ifelse(cl==k,1,0)
    }
    
    ##############################
    # M-step
    theta=colSums(Z)/n
    par_init=as.vector(Alpha)
    opt=optim(par=par_init,lower=rep(0.01,length(par_init)),fn=hybrid_ll,Xd=Xd,Z=Z,cl=cl,
              theta=theta,method="L-BFGS-B",control=list(fnscale=-1))
    a=opt$par
    Alpha=matrix(a,K,K,byrow=FALSE)
    #Alpha_list=c(Alpha_list,list(Alpha)) # If want to store Alpha estimates at each iteration
    
    llvals=c(llvals,obs_ll(Xd,cl,a,theta)) # Observed hybrid log-likelihood values
    #c_llvals=c(c_llvals,opt$value) # Complete data hybrid log-likelihood values
    ll_old=llvals[length(llvals)-1]
    ll_new=llvals[length(llvals)]
    if (length(llvals)==50) break
  }
  Z_cl_order=apply(Z,1,which.max)
  cl_order=matchClasses(table(cl,Z_cl_order),method="exact",verbose=0)
  cl_order=matrix(cl_order,2,K,byrow=TRUE)[1,]
  Alpha=Alpha[cl_order,]
  theta=theta[cl_order]
  
  outputs=list(Z_hard=Z_hard,Z=Z,cl=cl,theta=theta,Alpha=Alpha,llvals=llvals)
  return(outputs)
  # Outputs: Z_hard - binary cluster allocations matrix
  #          Z - cluster allocation probabilities matrix
  #          cl - vector of cluster labels
  #          theta - mixing proportions
  #          Alpha - Dirichlet parameters matrix estimate
  #          llvals - observed hybrid log-likelihood values
}
