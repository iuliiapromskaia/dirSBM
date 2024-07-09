# Initialisation strategy choice: simulation study

# Required packages
library(blockmodels)
library(igraph) # For data generation
library(kernlab)
library(mclust)
library(doParallel) # For repeated random initialisation
library(compositions)
library(RColorBrewer) # For plots
library(LaplacesDemon)

set.seed(937)
K=3
n=50
Alpha=matrix(c(1,0.7,0.5,0.9,1.5,0.6,0.4,0.5,1.2),nrow=K,ncol=K,byrow=TRUE)

n_reps=50

datasets=vector("list",n_reps)

init_random=rep(NA,n_reps)
init_kmeans=rep(NA,n_reps)
init_clr_kmeans=rep(NA,n_reps)
init_specc=rep(NA,n_reps)
init_sbm_bin=rep(NA,n_reps)
init_sbm_gaus=rep(NA,n_reps)

ll_random=rep(NA,n_reps)
ll_kmeans=rep(NA,n_reps)
ll_clr_kmeans=rep(NA,n_reps)
ll_specc=rep(NA,n_reps)
ll_sbm_bin=rep(NA,n_reps)
ll_sbm_gaus=rep(NA,n_reps)

cl_random=vector("list",n_reps)
cl_km=vector("list",n_reps)
cl_clr_km=vector("list",n_reps)
cl_specc=vector("list",n_reps)
cl_sbm_bin=vector("list",n_reps)
cl_sbm_gaus=vector("list",n_reps)

Alpha_random=vector("list",n_reps)
Alpha_kmeans=vector("list",n_reps)
Alpha_clr_kmeans=vector("list",n_reps)
Alpha_specc=vector("list",n_reps)
Alpha_sbm_bin=vector("list",n_reps)
Alpha_sbm_gaus=vector("list",n_reps)

time_spent=system.time({
  for (reps in 1:n_reps){
    print(reps)
    print(Sys.time())
    # Generate data
    synth_data=dirSBM_data_gen(n,K,Alpha)
    X=synth_data$X
    clusters_true=synth_data$clusters_true
    datasets[[reps]]=synth_data
    
    ### Random
    cl=makeCluster(detectCores()-2)
    #cl=makeCluster(5)
    registerDoParallel(cl)
    random=dirSBM_init_best(X,K,init="random",5)
    cl_random[[reps]]=random$cl
    init_random[reps]=adjustedRandIndex(random$cl,clusters_true)
    ll_random[reps]=tail(random$llvals,n=1)
    Alpha_random[[reps]]=random$Alpha
    stopCluster(cl)
    
    ### k-means
    km=dirSBM_init(X,K,init="kmeans")
    cl_km[[reps]]=km$cl
    init_kmeans[reps]=adjustedRandIndex(km$cl,clusters_true)
    ll_kmeans[reps]=tail(km$llvals,n=1)
    Alpha_kmeans[[reps]]=km$Alpha

    ### CLR + k-means
    clr_km=dirSBM_init(X,K,init="clr_kmeans")
    cl_clr_km[[reps]]=clr_km$cl
    init_clr_kmeans[reps]=adjustedRandIndex(clr_km$cl,clusters_true)
    ll_clr_kmeans[reps]=tail(clr_km$llvals,n=1)
    Alpha_clr_kmeans[[reps]]=clr_km$Alpha

    ### Spectral clustering
    sp=dirSBM_init(X,K,init="specc")
    cl_specc[[reps]]=sp$cl
    init_specc[reps]=adjustedRandIndex(sp$cl,clusters_true)
    ll_specc[reps]=tail(sp$llvals,n=1)
    Alpha_specc[[reps]]=sp$Alpha

    ### Binary SBM
    sbm_bin=dirSBM_init(X,K,init="sbm_bin")
    cl_sbm_bin[[reps]]=sbm_bin$cl
    init_sbm_bin[reps]=adjustedRandIndex(sbm_bin$cl,clusters_true)
    ll_sbm_bin[reps]=tail(sbm_bin$llvals,n=1)
    Alpha_sbm_bin[[reps]]=sbm_bin$Alpha

    ### Gaussian SBM
    sbm_gaus=dirSBM_init(X,K,init="sbm_gaus")
    cl_sbm_gaus[[reps]]=sbm_gaus$cl
    init_sbm_gaus[reps]=adjustedRandIndex(sbm_gaus$cl,clusters_true)
    ll_sbm_gaus[reps]=tail(sbm_gaus$llvals,n=1)
    Alpha_sbm_gaus[[reps]]=sbm_gaus$Alpha
  }
})


init_study_results=list(datasets=datasets,
                        init_random=init_random,
                        init_kmeans=init_kmeans,
                        init_clr_kmeans=init_clr_kmeans,
                        init_specc=init_specc,
                        init_sbm_bin=init_sbm_bin,
                        init_sbm_gaus=init_sbm_gaus,
                        ll_random=ll_random,
                        ll_kmeans=ll_kmeans,
                        ll_clr_kmeans=ll_clr_kmeans,
                        ll_specc=ll_specc,
                        ll_sbm_bin=ll_sbm_bin,
                        ll_sbm_gaus=ll_sbm_gaus,
                        cl_random=cl_random,
                        cl_km=cl_km,
                        cl_clr_km=cl_clr_km,
                        cl_specc=cl_specc,
                        cl_sbm_bin=cl_sbm_bin,
                        cl_sbm_gaus=cl_sbm_gaus,
                        Alpha_random=Alpha_random,
                        Alpha_kmeans=Alpha_kmeans,
                        Alpha_clr_kmeans=Alpha_clr_kmeans,
                        Alpha_specc=Alpha_specc,
                        Alpha_sbm_bin=Alpha_sbm_bin,
                        Alpha_sbm_gaus=Alpha_sbm_gaus,
                        time_spent=time_spent)

#save(init_study_results,file="init_study_results.RData") 
# Saved results available in dirSBM\Simulation studies\Initialisation_strategy_results

#pdf("Init_boxplot.pdf",width=1.4*7,height=7)
par(mfrow=c(1,1),oma = c(1.5,3,0,0) + 0.1,mar = c(3,1,0,0) + 0.5)
boxplot(init_study_results$init_random,init_study_results$init_kmeans,init_study_results$init_clr_kmeans,
        init_study_results$init_specc,init_study_results$init_sbm_bin,init_study_results$init_sbm_gaus,boxlwd=2,whisklwd=2,staplelwd=2,
        names=c("Random","K-means","CLR+","Spectral","BinSBM","GausSBM"),pch=19,
        main="",cex.main=1.35,cex.axis=1.4,cex.lab=1.2,ylim=c(0.27,1),col="white")
axis(1,at=1:6, c("","","K-means","clustering","",""),
     line=1.25, lwd=0, cex.axis=1.4)
m=c(mean(init_study_results$init_random),mean(init_study_results$init_kmeans),mean(init_study_results$init_clr_kmeans),
    mean(init_study_results$init_specc),mean(init_study_results$init_sbm_bin),mean(init_study_results$init_sbm_gaus))
points(m,col=brewer.pal(n=8,name="RdBu")[8],pch=19,cex=1.8)
text(1:6,m-0.035,labels=paste(round(m,3)),col=brewer.pal(n=8,name="RdBu")[8],cex=1.4,font=2)
mtext("        Adjusted Rand Index", side = 2, line = 1, outer = TRUE, cex=1.5)
mtext("Initialisation strategy", side = 1, line = 0.5, outer = TRUE, cex=1.5)
#dev.off()

lls_all=rbind(init_study_results$ll_random,init_study_results$ll_kmeans,init_study_results$ll_clr_kmeans,init_study_results$ll_specc,
              init_study_results$ll_sbm_bin,init_study_results$ll_sbm_gaus)

# Order initialisation strategies according to performance in terms of observed hybrid log-likelihood value
# 1 = random, 2 = kmeans, 3 = clr+kmeans, 4 = specc, 5 = sbm_bin, 6 = sbm_gaus
ordered_ll=apply(lls_all,2,function(lls_all) order(lls_all, decreasing = TRUE))

table(ordered_ll[1,]) # Best
table(ordered_ll[2,])
table(ordered_ll[3,])
table(ordered_ll[4,])
table(ordered_ll[5,])
table(ordered_ll[6,]) # Worst

# Order initialisation strategies according to performance in terms of adjusted Rand index value
# 1 = random, 2 = kmeans, 3 = clr+kmeans, 4 = specc, 5 = sbm_bin, 6 = sbm_gaus
ari_all=rbind(init_study_results$init_random,init_study_results$init_kmeans,init_study_results$init_clr_kmeans,init_study_results$init_specc,
              init_study_results$init_sbm_bin,init_study_results$init_sbm_gaus)

ordered_ari=apply(ari_all,2,function(ari_all) order(ari_all, decreasing = TRUE))

table(ordered_ari[1,]) # Best
table(ordered_ari[2,])
table(ordered_ari[3,])
table(ordered_ari[4,])
table(ordered_ari[5,])
table(ordered_ari[6,]) # Worst
