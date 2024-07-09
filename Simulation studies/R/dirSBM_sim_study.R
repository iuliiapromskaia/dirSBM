# Parameter estimation quality simulation study: fit DirSBM with known number of clusters to assess
# parameter value recovery. Based on 50 synthetic networks. The algorithm is initialised at random 
# 5 times, and the best run is considered for assessment.

dirSBM_sim_study=function(K,n,Alpha){
  # Inputs: K - number of clusters
  #         n - number of nodes/observations
  #         Alpha - Dirichlet parameter matrix
  
  n_reps=50 # Number of synthetic data sets to generate
  datasets=vector("list",n_reps)
  cl_mat=matrix(NA,n_reps,n)
  Alpha_list=vector("list",n_reps)
  timing=rep(NA,n_reps)
  
  for (reps in 1:n_reps){
    print(reps)
    print(Sys.time())
    # Generate data
    synth_data=dirSBM_data_gen(n,K,Alpha)
    datasets[[reps]]=synth_data
    X=synth_data$X

    to_time=system.time({
      dirSBM_res=dirSBM(X,K)
      cl_mat[reps,]=dirSBM_res$cl
      Alpha_list[[reps]]=dirSBM_res$Alpha
    })
    timing[reps]=to_time[["elapsed"]]
  }
  
  outs=list(datasets=datasets,cl_mat=cl_mat,Alpha_list=Alpha_list,timing=timing)
  return(outs)
  # Outputs: datasets - list of artificial data sets used in the simulation study 
  #                     ($X - data matrix, $clusters_true - true cluster labels)
  #          cl_mat - matrix of cluster labels obtained (row per data set)
  #          Alpha_list - Dirichlet parameter matrix estimates for each synthetic data set
  #          timing - time taken in seconds to fit Dirichlet SBM
}