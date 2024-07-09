# Clustering performance simulation study: comparing binary SBM, Gaussian SBM on untransformed
# data, Gaussian SBM on log-ratio transformed data and Dirichlet SBM. Dirichlet SBM is
# initialised at random (5 times). Number of clusters K is assumed to be known and is fixed

sim_study=function(K,n,Alpha){
  # Inputs: K - number of clusters
  #         n - number of nodes/observations
  #         Alpha - Dirichlet parameter matrix

  n_reps=50 # Number of synthetic data sets to generate

  ### Competing models
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
      adj=as.matrix(X) # Centered log-ratio transformed data
      sbm=BM_gaussian(membership_type="SBM",adj=adj,verbosity=0,plotting="",explore_min=K,explore_max=K)
      sbm$estimate()
      clusters_g_sbm_naive=apply(sbm$memberships[[K]]$Z,1,which.max)
      return(clusters_g_sbm_naive)
    }

    # Gaussian SBM on log-ratio transformed data
    gaus_sbm=function(X,K){
      adj=as.matrix(clr(X)) # Centered log-ratio transformed data
      sbm=BM_gaussian(membership_type="SBM",adj=adj,verbosity=0,plotting="",explore_min=K,explore_max=K)
      sbm$estimate()
      clusters_g_sbm=apply(sbm$memberships[[K]]$Z,1,which.max)
      return(clusters_g_sbm)
    }
  ###
  
  datasets=vector("list",n_reps)
  clusters_sbm=matrix(NA,n_reps,n)
  clusters_g_sbm=matrix(NA,n_reps,n)
  clusters_g_sbm_naive=matrix(NA,n_reps,n)
  clusters_dir_sbm=matrix(NA,n_reps,n)
  sbm_ARand=rep(NA,n_reps)
  g_sbm_ARand=rep(NA,n_reps)
  g_sbm_naive_ARand=rep(NA,n_reps)
  dir_sbm_ARand=rep(NA,n_reps)
  Alpha_list=vector("list",n_reps)
  timing=rep(NA,n_reps)
  for (reps in 1:n_reps){
    print(reps)
    print(Sys.time())
    # Generate data
    synth_data=dirSBM_data_gen(n,K,Alpha)
    datasets[[reps]]=synth_data
    X=synth_data$X
    clusters_true=synth_data$clusters_true

    # Fit models
    clusters_sbm[reps,]=binary_sbm(X,K)
    clusters_g_sbm_naive[reps,]=gaus_sbm_naive(X,K)
    clusters_g_sbm[reps,]=gaus_sbm(X,K)

    to_time=system.time({
      dirSBM_res=dirSBM(X,K)
      clusters_dir_sbm[reps,]=dirSBM_res$cl
      Alpha_list[[reps]]=dirSBM_res$Alpha
    })
    timing[reps]=to_time[["elapsed"]]
    
    # Compute Adjusted Rand index (ARI) for all
    sbm_ARand[reps]=adjustedRandIndex(clusters_sbm[reps,],clusters_true)
    g_sbm_naive_ARand[reps]=adjustedRandIndex(clusters_g_sbm_naive[reps,],clusters_true)
    g_sbm_ARand[reps]=adjustedRandIndex(clusters_g_sbm[reps,],clusters_true)
    dir_sbm_ARand[reps]=adjustedRandIndex(clusters_dir_sbm[reps,],clusters_true)
  }

  outs=list(datasets=datasets,clusters_sbm=clusters_sbm,clusters_g_sbm_naive=clusters_g_sbm_naive,clusters_g_sbm=clusters_g_sbm,
            clusters_dir_sbm=clusters_dir_sbm, sbm_ARand=sbm_ARand,g_sbm_naive_ARand=g_sbm_naive_ARand,g_sbm_ARand=g_sbm_ARand,
            dir_sbm_ARand=dir_sbm_ARand,Alpha_list=Alpha_list,timing=timing)
  return(outs)
  # Outputs: datasets - list of artificial data sets used in the simulation study 
  #                     ($X - data matrix, $clusters_true - true cluster labels)
  #          clusters_sbm - clustering solutions of binary SBM
  #          clusters_g_sbm_naive - clustering solutions of Gaussian SBM on untransformed data
  #          clusters_g_sbm - clustering solutions of Gaussian SBM on CLR-transformed data
  #          clusters_dir_sbm - clustering solutions of Dirichlet SBM
  #          sbm_ARand - ARI of binary SBM
  #          g_sbm_naive_ARand - ARI of Gaussian SBM on untransformed data
  #          g_sbm_ARand - ARI of Gaussian SBM on log-ratio transformed data
  #          dir_sbm_ARand - ARI of Dirichlet SBM
  #          Alpha_list - Dirichlet parameter matrix estimates for each synthetic data set
  #          timing - time taken in seconds to fit Dirichlet SBM
}
