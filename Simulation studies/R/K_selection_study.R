# Model selection simulation study: assessing model selection performance of Integrated Completed
# Likelihood (ICL) for Dirichlet SBM. Generating 50 synthetic data sets and fitting the models
# with k=1,...,K_max clusters. Optimal k has the highest icl_vals.

K_selection_study=function(n,K,Alpha,K_max){
  # Inputs: n - number of nodes/observations
  #         K - true number of clusetrs used to generate synthetic data sets
  #         Alpha - true Dirichlet parameter matrix used to generate synthetc data sets
  #         K_max - maximum number of clusters to fit Dirichlet SBM with
  
  n_reps=50 # Number of syntheic data sets to generate
  icl_vals=matrix(NA,nrow=n_reps,ncol=K_max)
  h_ll_vals=matrix(NA,nrow=n_reps,ncol=K_max)
  runtimes=matrix(NA,nrow=n_reps,ncol=K_max)
  Alpha_est=vector("list",n_reps)
  cl_est=vector("list",n_reps)
  
  # Store original clustering to help identify label switching later on
  cl_true=matrix(NA,nrow=n_reps,ncol=n)
  
  for (reps in 1:n_reps){
    # Print to track progress
    print(reps)
    print(Sys.time())
    
    # Generate synthetic data
    synth_data=dirSBM_data_gen(n,K,Alpha)
    X=synth_data$X
    clusters_true=synth_data$clusters_true
    cl_true[reps,]=clusters_true # Store true cluster labels
    
    icl=K_selection(X,K_max)
    icl_vals[reps,]=icl$ICL # Store the model selection criterion value
    h_ll_vals[reps,]=icl$h_ll # Store the value of complete data hybrid log-likelihood
    runtimes[reps,]=icl$runtime # Store the running times for each value of k for each data set
    Alpha_est[[reps]]=icl$Alphas # Store the Dirichlet parameter matrix estimates for each k
                                 # and each data set
    cl_est[[reps]]=icl$cls # Store cluster labels for each k and each data set
  }
  outs=list(icl_vals=icl_vals,h_ll_vals=h_ll_vals,runtimes=runtimes,cl_true=cl_true,cl_est=cl_est,
            Alpha_est=Alpha_est)
  return(outs)
  # Outputs: icl_vals - matrix of Integrated Completed Likelihood (ICL) values, one row per synthetic
  #                     data set with 1,...,K_max values
  #          h_ll_vals - matrix of the complete data hybrid log-likelihood values, one row per
  #                      synthetic data set with 1,...,K_max values
  #          runtimes - matrix of running times of the algorithm, one row per synthetic data set with
  #                     1,...,K_max values
  #          cl_true - matrix of true cluster labels, row per data set (useful for label switching)
  #          cl_est - list of matrices of estimated cluster labels, list entries correspond to data sets,
  #                   rows of a matrix correspond to each of the k=1,...,K_max solutions
  #          Alpha_est - list of lists of matrices, Alpha_est[[i]][[j]] gives the estimate of Dirichlet
  #                      parameter matrix for i-th data set with j clusters
}
