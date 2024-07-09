library(igraph)
library(RColorBrewer)
library(kernlab)
library(doParallel)
library(LaplacesDemon)
library(e1071)
library(RColorBrewer)
library(latex2exp)

################
##### K=2 ######
################

### Small
set.seed(4793)
K=2
n=30
Alpha=matrix(c(1,0.6,0.8,1.5),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
s230=dirSBM_sim_study(K,n,Alpha)
#save(s230,file="s230.RData")
stopCluster(cl)

set.seed(256762) 
K=2
n=50
Alpha=matrix(c(1,0.6,0.8,1.5),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
s250=dirSBM_sim_study(K,n,Alpha)
#save(s250,file="s250.RData")
stopCluster(cl)

set.seed(8756)
K=2
n=70
Alpha=matrix(c(1,0.6,0.8,1.5),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
s270=dirSBM_sim_study(K,n,Alpha)
#save(s270,file="s270.RData")
stopCluster(cl)

set.seed(75358)
K=2
n=100
Alpha=matrix(c(1,0.6,0.8,1.5),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
s2100=dirSBM_sim_study(K,n,Alpha)
#save(s2100,file="s2100.RData")
stopCluster(cl)



### Medium
set.seed(47837)
K=2
n=30
Alpha=matrix(c(1,0.6,0.9,1.4),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
m230=dirSBM_sim_study(K,n,Alpha)
#save(m230,file="m230.RData")
stopCluster(cl)

set.seed(83011)
K=2
n=50
Alpha=matrix(c(1,0.6,0.9,1.4),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
m250=dirSBM_sim_study(K,n,Alpha)
#save(m250,file="m250.RData")
stopCluster(cl)

set.seed(2754)
K=2
n=70
Alpha=matrix(c(1,0.6,0.9,1.4),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
m270=dirSBM_sim_study(K,n,Alpha)
#save(m270,file="m270.RData")
stopCluster(cl)

set.seed(3573)
K=2
n=100
Alpha=matrix(c(1,0.6,0.9,1.4),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
m2100=dirSBM_sim_study(K,n,Alpha)
#save(m2100,file="m2100.RData")
stopCluster(cl)


### Large
set.seed(84651)
K=2
n=30
Alpha=matrix(c(1,0.8,0.9,1.5),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
l230=dirSBM_sim_study(K,n,Alpha)
#save(l230,file="l230.RData")
stopCluster(cl)

set.seed(884465)
K=2
n=50
Alpha=matrix(c(1,0.8,0.9,1.5),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
l250=dirSBM_sim_study(K,n,Alpha)
#save(l250,file="l250.RData")
stopCluster(cl)

set.seed(10192)
K=2
n=70
Alpha=matrix(c(1,0.8,0.9,1.5),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
l270=dirSBM_sim_study(K,n,Alpha)
#save(l270,file="l270.RData")
stopCluster(cl)

set.seed(99563) 
K=2
n=100
Alpha=matrix(c(1,0.8,0.9,1.5),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
l2100=dirSBM_sim_study(K,n,Alpha)
#save(l2100,file="l2100.RData")
stopCluster(cl)


################
##### K=3 ######
################

### Small
set.seed(61793)
K=3
n=30
Alpha=matrix(c(1,0.6,0.2,0.6,1.5,0.5,0.3,0.4,1.2),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
s330=dirSBM_sim_study(K,n,Alpha)
#save(s330,file="s330.RData")
stopCluster(cl)

set.seed(8589)
K=3
n=50
Alpha=matrix(c(1,0.6,0.2,0.6,1.5,0.5,0.3,0.4,1.2),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
s350=dirSBM_sim_study(K,n,Alpha)
#save(s350,file="s350.RData")
stopCluster(cl)

set.seed(752)
K=3
n=70
Alpha=matrix(c(1,0.6,0.2,0.6,1.5,0.5,0.3,0.4,1.2),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
s370=dirSBM_sim_study(K,n,Alpha)
#save(s370,file="s370.RData")
stopCluster(cl)

set.seed(354) 
K=3
n=100
Alpha=matrix(c(1,0.6,0.2,0.6,1.5,0.5,0.3,0.4,1.2),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
s3100=dirSBM_sim_study(K,n,Alpha)
#save(s3100,file="s3100.RData")
stopCluster(cl)

### Medium
set.seed(9876)
K=3
n=30
Alpha=matrix(c(1,0.7,0.5,0.9,1.5,0.6,0.4,0.5,1.2),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
m330=dirSBM_sim_study(K,n,Alpha)
#save(m330,file="m330.RData")
stopCluster(cl)

set.seed(7451)
K=3
n=50
Alpha=matrix(c(1,0.7,0.5,0.9,1.5,0.6,0.4,0.5,1.2),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
m350=dirSBM_sim_study(K,n,Alpha)
#save(m350,file="m350.RData")
stopCluster(cl)

set.seed(385)
K=3
n=70
Alpha=matrix(c(1,0.7,0.5,0.9,1.5,0.6,0.4,0.5,1.2),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
m370=dirSBM_sim_study(K,n,Alpha)
#save(m370,file="m370.RData")
stopCluster(cl)

set.seed(1959) 
K=3
n=100
Alpha=matrix(c(1,0.7,0.5,0.9,1.5,0.6,0.4,0.5,1.2),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
m3100=dirSBM_sim_study(K,n,Alpha)
#save(m3100,file="m3100.RData")
stopCluster(cl)


### Large
set.seed(546576)
K=3
n=30
Alpha=matrix(c(1,0.7,0.5,0.9,1.3,0.7,0.6,0.5,1.2),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
l330=dirSBM_sim_study(K,n,Alpha)
#save(l330,file="l330.RData")
stopCluster(cl)

set.seed(836) 
K=3
n=50
Alpha=matrix(c(1,0.7,0.5,0.9,1.3,0.7,0.6,0.5,1.2),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
l350=dirSBM_sim_study(K,n,Alpha)
#save(l350,file="l350.RData")
stopCluster(cl)

set.seed(76) 
K=3
n=70
Alpha=matrix(c(1,0.7,0.5,0.9,1.3,0.7,0.6,0.5,1.2),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
l370=dirSBM_sim_study(K,n,Alpha)
#save(l370,file="l370.RData")
stopCluster(cl)

set.seed(293) 
K=3
n=100
Alpha=matrix(c(1,0.7,0.5,0.9,1.3,0.7,0.6,0.5,1.2),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
l3100=dirSBM_sim_study(K,n,Alpha)
#save(l3100,file="l3100.RData")
stopCluster(cl)


################
##### K=5 ######
################

### Small
set.seed(83571)
K=5
n=30
Alpha=matrix(c(1,0.6,0.2,0.3,0.5,0.6,1.5,0.5,0.4,0.7,0.3,0.4,1.2,0.5,0.2,0.7,0.5,
               0.3,1.4,0.4,0.5,0.7,0.8,0.6,1.7),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
s530=dirSBM_sim_study(K,n,Alpha)
#save(s530,file="s530.RData")
stopCluster(cl)

set.seed(7654)
K=5
n=50
Alpha=matrix(c(1,0.6,0.2,0.3,0.5,0.6,1.5,0.5,0.4,0.7,0.3,0.4,1.2,0.5,0.2,0.7,0.5,
               0.3,1.4,0.4,0.5,0.7,0.8,0.6,1.7),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
s550=dirSBM_sim_study(K,n,Alpha)
#save(s550,file="s550.RData")
stopCluster(cl)

set.seed(836012)
K=5
n=70
Alpha=matrix(c(1,0.6,0.2,0.3,0.5,0.6,1.5,0.5,0.4,0.7,0.3,0.4,1.2,0.5,0.2,0.7,0.5,
               0.3,1.4,0.4,0.5,0.7,0.8,0.6,1.7),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
s570=dirSBM_sim_study(K,n,Alpha)
#save(s570,file="s570.RData")
stopCluster(cl)

set.seed(9815003)
K=5
n=100
Alpha=matrix(c(1,0.6,0.2,0.3,0.5,0.6,1.5,0.5,0.4,0.7,0.3,0.4,1.2,0.5,0.2,0.7,0.5,
               0.3,1.4,0.4,0.5,0.7,0.8,0.6,1.7),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
s5100=dirSBM_sim_study(K,n,Alpha)
#save(s5100,file="s5100.RData")
stopCluster(cl)

### Medium
set.seed(18234678)
K=5
n=30
Alpha=matrix(c(1,0.7,0.5,0.4,0.6,0.9,1.5,0.6,0.5,0.7,0.4,0.5,1.2,0.6,0.3,0.8,0.6,
               0.4,1.4,0.5,0.5,0.8,0.9,0.7,1.7),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
m530=dirSBM_sim_study(K,n,Alpha)
#save(m530,file="m530.RData")
stopCluster(cl)

set.seed(9070008)
K=5
n=50
Alpha=matrix(c(1,0.7,0.5,0.4,0.6,0.9,1.5,0.6,0.5,0.7,0.4,0.5,1.2,0.6,0.3,0.8,0.6,
               0.4,1.4,0.5,0.5,0.8,0.9,0.7,1.7),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
m550=dirSBM_sim_study(K,n,Alpha)
#save(m550,file="m550.RData")
stopCluster(cl)

set.seed(8940111)
K=5
n=70
Alpha=matrix(c(1,0.7,0.5,0.4,0.6,0.9,1.5,0.6,0.5,0.7,0.4,0.5,1.2,0.6,0.3,0.8,0.6,
               0.4,1.4,0.5,0.5,0.8,0.9,0.7,1.7),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
m570=dirSBM_sim_study(K,n,Alpha)
#save(m570,file="m570.RData")
stopCluster(cl)

set.seed(7774456)
K=5
n=100
Alpha=matrix(c(1,0.7,0.5,0.4,0.6,0.9,1.5,0.6,0.5,0.7,0.4,0.5,1.2,0.6,0.3,0.8,0.6,
               0.4,1.4,0.5,0.5,0.8,0.9,0.7,1.7),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
m5100=dirSBM_sim_study(K,n,Alpha)
#save(m5100,file="m5100.RData")
stopCluster(cl)


### Large
set.seed(54634)
K=5
n=30
Alpha=matrix(c(1.0,0.7,0.5,0.4,0.6,0.9,1.3,0.7,0.5,0.8,0.6,0.7,1.2,0.8,0.5,0.8,0.6,
               0.4,1.4,0.7,0.7,0.8,0.9,0.6,1.6),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
l530=dirSBM_sim_study(K,n,Alpha)
#save(l530,file="l530.RData")
stopCluster(cl)

set.seed(7000503)
K=5
n=50
Alpha=matrix(c(1.0,0.7,0.5,0.4,0.6,0.9,1.3,0.7,0.5,0.8,0.6,0.7,1.2,0.8,0.5,0.8,0.6,
               0.4,1.4,0.7,0.7,0.8,0.9,0.6,1.6),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
l550=dirSBM_sim_study(K,n,Alpha)
#save(l550,file="l550.RData")
stopCluster(cl)

set.seed(199957)
K=5
n=70
Alpha=matrix(c(1.0,0.7,0.5,0.4,0.6,0.9,1.3,0.7,0.5,0.8,0.6,0.7,1.2,0.8,0.5,0.8,0.6,
               0.4,1.4,0.7,0.7,0.8,0.9,0.6,1.6),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
l570=dirSBM_sim_study(K,n,Alpha)
#save(l570,file="l570.RData")
stopCluster(cl)

set.seed(4442217)
K=5
n=100
Alpha=matrix(c(1.0,0.7,0.5,0.4,0.6,0.9,1.3,0.7,0.5,0.8,0.6,0.7,1.2,0.8,0.5,0.8,0.6,
               0.4,1.4,0.7,0.7,0.8,0.9,0.6,1.6),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
l5100=dirSBM_sim_study(K,n,Alpha)
#save(l5100,file="l5100.RData")
stopCluster(cl)


###################################################################################################################

# Calculate Frobenius distances between true parameter matrix Alpha and its estimates

### K=2
K=2
# Small
Alpha=matrix(c(1,0.6,0.8,1.5),nrow=K,ncol=K,byrow=TRUE)

fd_s230=rep(NA,50)
for (reps in 1:50){
  label_swap=matchClasses(table(s230$datasets[[reps]]$clusters_true,s230$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_s230[reps]=norm(s230$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
}

fd_s250=rep(NA,50)
for (reps in 1:50){
  label_swap=matchClasses(table(s250$datasets[[reps]]$clusters_true,s250$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_s250[reps]=norm(s250$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
}

fd_s270=rep(NA,50)
for (reps in 1:50){
  label_swap=matchClasses(table(s270$datasets[[reps]]$clusters_true,s270$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_s270[reps]=norm(s270$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
}

fd_s2100=rep(NA,50)
for (reps in 1:50){
  label_swap=matchClasses(table(s2100$datasets[[reps]]$clusters_true,s2100$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_s2100[reps]=norm(s2100$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
}

# Medium
Alpha=matrix(c(1,0.6,0.9,1.4),nrow=K,ncol=K,byrow=TRUE)

fd_m230=rep(NA,50)
for (reps in 1:50){
  label_swap=matchClasses(table(m230$datasets[[reps]]$clusters_true,m230$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_m230[reps]=norm(m230$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
}

fd_m250=rep(NA,50)
for (reps in 1:50){
  label_swap=matchClasses(table(m250$datasets[[reps]]$clusters_true,m250$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_m250[reps]=norm(m250$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
}

fd_m270=rep(NA,50)
for (reps in 1:50){
  label_swap=matchClasses(table(m270$datasets[[reps]]$clusters_true,m270$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_m270[reps]=norm(m270$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
}

fd_m2100=rep(NA,50)
for (reps in 1:50){
  label_swap=matchClasses(table(m2100$datasets[[reps]]$clusters_true,m2100$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_m2100[reps]=norm(m2100$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
}

# Large
Alpha=matrix(c(1,0.8,0.9,1.5),nrow=K,ncol=K,byrow=TRUE)

fd_l230=rep(NA,50)
for (reps in 1:50){
  label_swap=matchClasses(table(l230$datasets[[reps]]$clusters_true,l230$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_l230[reps]=norm(l230$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
}

fd_l250=rep(NA,50)
for (reps in 1:50){
  label_swap=matchClasses(table(l250$datasets[[reps]]$clusters_true,l250$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_l250[reps]=norm(l250$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
}

fd_l270=rep(NA,50)
for (reps in 1:50){
  label_swap=matchClasses(table(l270$datasets[[reps]]$clusters_true,l270$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_l270[reps]=norm(l270$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
}

fd_l2100=rep(NA,50)
for (reps in 1:50){
  label_swap=matchClasses(table(l2100$datasets[[reps]]$clusters_true,l2100$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_l2100[reps]=norm(l2100$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
}


### K=3
K=3
# Small
Alpha=matrix(c(1,0.6,0.2,0.6,1.5,0.5,0.3,0.4,1.2),nrow=K,ncol=K,byrow=TRUE)

fd_s330=rep(NA,50)
for (reps in 1:50){
  label_swap=matchClasses(table(s330$datasets[[reps]]$clusters_true,s330$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_s330[reps]=norm(s330$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
}

fd_s350=rep(NA,50)
for (reps in 1:50){
  label_swap=matchClasses(table(s350$datasets[[reps]]$clusters_true,s350$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_s350[reps]=norm(s350$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
}

fd_s370=rep(NA,50)
for (reps in 1:50){
  label_swap=matchClasses(table(s370$datasets[[reps]]$clusters_true,s370$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_s370[reps]=norm(s370$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
}

fd_s3100=rep(NA,50)
for (reps in 1:50){
  label_swap=matchClasses(table(s3100$datasets[[reps]]$clusters_true,s3100$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_s3100[reps]=norm(s3100$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
}

# Medium
Alpha=matrix(c(1,0.7,0.5,0.9,1.5,0.6,0.4,0.5,1.2),nrow=K,ncol=K,byrow=TRUE)

fd_m330=rep(NA,50)
for (reps in 1:50){
  label_swap=matchClasses(table(m330$datasets[[reps]]$clusters_true,m330$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_m330[reps]=norm(m330$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
}

fd_m350=rep(NA,50)
for (reps in 1:50){
  label_swap=matchClasses(table(m350$datasets[[reps]]$clusters_true,m350$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_m350[reps]=norm(m350$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
}

fd_m370=rep(NA,50)
for (reps in 1:50){
  label_swap=matchClasses(table(m370$datasets[[reps]]$clusters_true,m370$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_m370[reps]=norm(m370$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
}

fd_m3100=rep(NA,50)
for (reps in 1:50){
  label_swap=matchClasses(table(m3100$datasets[[reps]]$clusters_true,m3100$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_m3100[reps]=norm(m3100$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
}

# Large
Alpha=matrix(c(1,0.7,0.5,0.9,1.3,0.7,0.6,0.5,1.2),nrow=K,ncol=K,byrow=TRUE)

fd_l330=rep(NA,50)
for (reps in 1:50){
  label_swap=matchClasses(table(l330$datasets[[reps]]$clusters_true,l330$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_l330[reps]=norm(l330$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
}

fd_l350=rep(NA,50)
for (reps in 1:50){
  label_swap=matchClasses(table(l350$datasets[[reps]]$clusters_true,l350$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_l350[reps]=norm(l350$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
}

fd_l370=rep(NA,50)
for (reps in 1:50){
  label_swap=matchClasses(table(l370$datasets[[reps]]$clusters_true,l370$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_l370[reps]=norm(l370$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
}

fd_l3100=rep(NA,50)
for (reps in 1:50){
  label_swap=matchClasses(table(l3100$datasets[[reps]]$clusters_true,l3100$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_l3100[reps]=norm(l3100$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
}



### K=5
K=5
# Small
Alpha=matrix(c(1,0.6,0.2,0.3,0.5,0.6,1.5,0.5,0.4,0.7,0.3,0.4,1.2,0.5,0.2,0.7,0.5,
               0.3,1.4,0.4,0.5,0.7,0.8,0.6,1.7),nrow=K,ncol=K,byrow=TRUE)

fd_s530=rep(NA,50)
for (reps in 1:50){
  label_swap=matchClasses(table(s530$datasets[[reps]]$clusters_true,s530$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_s530[reps]=norm(s530$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
}

fd_s550=rep(NA,50)
for (reps in 1:50){
  label_swap=matchClasses(table(s550$datasets[[reps]]$clusters_true,s550$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_s550[reps]=norm(s550$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
}

fd_s570=rep(NA,50)
for (reps in 1:50){
  label_swap=matchClasses(table(s570$datasets[[reps]]$clusters_true,s570$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_s570[reps]=norm(s570$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
}

fd_s5100=rep(NA,50)
for (reps in 1:50){
  label_swap=matchClasses(table(s5100$datasets[[reps]]$clusters_true,s5100$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_s5100[reps]=norm(s5100$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
}

# Medium
Alpha=matrix(c(1,0.7,0.5,0.4,0.6,0.9,1.5,0.6,0.5,0.7,0.4,0.5,1.2,0.6,0.3,0.8,0.6,
               0.4,1.4,0.5,0.5,0.8,0.9,0.7,1.7),nrow=K,ncol=K,byrow=TRUE)

fd_m530=rep(NA,50)
for (reps in 1:50){
  label_swap=matchClasses(table(m530$datasets[[reps]]$clusters_true,m530$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_m530[reps]=norm(m530$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
}

fd_m550=rep(NA,50)
for (reps in 1:50){
  label_swap=matchClasses(table(m550$datasets[[reps]]$clusters_true,m550$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_m550[reps]=norm(m550$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
}

fd_m570=rep(NA,50)
for (reps in 1:50){
  label_swap=matchClasses(table(m570$datasets[[reps]]$clusters_true,m570$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_m570[reps]=norm(m570$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
}

fd_m5100=rep(NA,50)
for (reps in 1:50){
  label_swap=matchClasses(table(m5100$datasets[[reps]]$clusters_true,m5100$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_m5100[reps]=norm(m5100$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
}

# Large
Alpha=matrix(c(1.0,0.7,0.5,0.4,0.6,0.9,1.3,0.7,0.5,0.8,0.6,0.7,1.2,0.8,0.5,0.8,0.6,
               0.4,1.4,0.7,0.7,0.8,0.9,0.6,1.6),nrow=K,ncol=K,byrow=TRUE)

fd_l530=rep(NA,50)
for (reps in 1:50){
  if (reps!=18){ # 18th data set was generated without cluster 3, so excluded
  label_swap=matchClasses(table(l530$datasets[[reps]]$clusters_true,l530$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_l530[reps]=norm(l530$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
  }
}
fd_l530=fd_l530[!is.na(fd_l530)]

fd_l550=rep(NA,50)
for (reps in 1:50){
  label_swap=matchClasses(table(l550$datasets[[reps]]$clusters_true,l550$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_l550[reps]=norm(l550$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
}

fd_l570=rep(NA,50)
for (reps in 1:50){
  label_swap=matchClasses(table(l570$datasets[[reps]]$clusters_true,l570$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_l570[reps]=norm(l570$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
}

fd_l5100=rep(NA,50)
for (reps in 1:50){
  label_swap=matchClasses(table(l5100$datasets[[reps]]$clusters_true,l5100$cl_mat[reps,]),method="exact",verbose=0)
  label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]
  fd_l5100[reps]=norm(l5100$Alpha_list[[reps]][label_swap,label_swap]-Alpha,type="F")
}


################################### Plots


##### K=2
#pdf("param_rec_K=2.pdf",width=2.7*4.5,height=4.5)
par(mfrow=c(1,3),oma = c(4.5,4.5,0.5,0.5) + 0.1,mar = c(1,2,3,0))

boxplot(fd_s230,fd_s250,fd_s270,fd_s2100,
        names=c("","","",""),col=brewer.pal(n=8,name="RdBu")[7],pch=19,outcol=brewer.pal(n=8,name="RdBu")[7],
        boxlwd=2,whisklwd=2,staplelwd=2,cex=1.3,
        cex.main=2.5,cex.axis=2,cex.lab=2,main="Low homogeneity",ylim=c(0,3.6),frame=T)
axis(1,at=1:4,c("30","50","70","100"),line=0.6, lwd=0, cex.axis=2)
boxplot(fd_m230,fd_m250,fd_m270,fd_m2100,
        names=c("","","",""),col=brewer.pal(n=8,name="RdBu")[7],pch=19,outcol=brewer.pal(n=8,name="RdBu")[7],
        boxlwd=2,whisklwd=2,staplelwd=2,cex=1.3,
        cex.main=2.5,cex.axis=2,cex.lab=2,main="Medium homogeneity",ylim=c(0,3.6),frame=T,yaxt="n")
axis(1,at=1:4,c("30","50","70","100"),line=0.6, lwd=0, cex.axis=2)
boxplot(fd_l230,fd_l250,fd_l270,fd_l2100,
        names=c("","","",""),col=brewer.pal(n=8,name="RdBu")[7],pch=19,outcol=brewer.pal(n=8,name="RdBu")[7],
        boxlwd=2,whisklwd=2,staplelwd=2,cex=1.3,
        cex.main=2.5,cex.axis=2,cex.lab=2,main="High homogeneity",ylim=c(0,3.6),frame=T,yaxt="n")
axis(1,at=1:4,c("30","50","70","100"),line=0.6, lwd=0, cex.axis=2)
mtext("Number of nodes, n", side = 1, line = 3, outer = TRUE, font=1, cex=1.6)
mtext(TeX("$|| \\textbf{A} - \\textbf{\\hat{A}} ||_F$"), side = 2, line = 1.3, outer = TRUE, font=1, cex=1.6)
#dev.off()


##### K=3
#pdf("param_rec_K=3.pdf",width=2.7*4.5,height=4.5)
par(mfrow=c(1,3),oma = c(4.5,4.5,0.5,0.5) + 0.1,mar = c(1,2,3,0))

boxplot(fd_s330,fd_s350,fd_s370,fd_s3100,
        names=c("","","",""),col=brewer.pal(n=8,name="RdBu")[7],pch=19,outcol=brewer.pal(n=8,name="RdBu")[7],
        boxlwd=2,whisklwd=2,staplelwd=2,cex=1.3,
        cex.main=2.5,cex.axis=2,cex.lab=2,main="Low homogeneity",ylim=c(0,3.6),frame=T)
axis(1,at=1:4,c("30","50","70","100"),line=0.6, lwd=0, cex.axis=2)
boxplot(fd_m330,fd_m350,fd_m370,fd_m3100,
        names=c("","","",""),col=brewer.pal(n=8,name="RdBu")[7],pch=19,outcol=brewer.pal(n=8,name="RdBu")[7],
        boxlwd=2,whisklwd=2,staplelwd=2,cex=1.3,
        cex.main=2.5,cex.axis=2,cex.lab=2,main="Medium homogeneity",ylim=c(0,3.6),frame=T,yaxt="n")
axis(1,at=1:4,c("30","50","70","100"),line=0.6, lwd=0, cex.axis=2)
boxplot(fd_l330,fd_l350,fd_l370,fd_l3100,
        names=c("","","",""),col=brewer.pal(n=8,name="RdBu")[7],pch=19,outcol=brewer.pal(n=8,name="RdBu")[7],
        boxlwd=2,whisklwd=2,staplelwd=2,cex=1.3,
        cex.main=2.5,cex.axis=2,cex.lab=2,main="High homogeneity",ylim=c(0,3.6),frame=T,yaxt="n")
axis(1,at=1:4,c("30","50","70","100"),line=0.6, lwd=0, cex.axis=2)
mtext("Number of nodes, n", side = 1, line = 3, outer = TRUE, font=1, cex=1.6)
mtext(TeX("$|| \\textbf{A} - \\textbf{\\hat{A}} ||_F$"), side = 2, line = 1.3, outer = TRUE, font=1, cex=1.6)
#dev.off()


##### K=5
#pdf("param_rec_K=5.pdf",width=2.7*4.5,height=4.5*1.2)
par(mfrow=c(1,3),oma = c(4.5,4.5,0.5,0.5) + 0.1,mar = c(1,2,3,0))

boxplot(fd_s530,fd_s550,fd_s570,fd_s5100,
        names=c("","","",""),col=brewer.pal(n=8,name="RdBu")[7],pch=19,outcol=brewer.pal(n=8,name="RdBu")[7],
        boxlwd=2,whisklwd=2,staplelwd=2,cex=1.3,
        cex.main=2.5,cex.axis=2,cex.lab=2,main="Low homogeneity",ylim=c(0,4.32),frame=T)
axis(1,at=1:4,c("30","50","70","100"),line=0.6, lwd=0, cex.axis=2)
boxplot(fd_m530,fd_m550,fd_m570,fd_m5100,
        names=c("","","",""),col=brewer.pal(n=8,name="RdBu")[7],pch=19,outcol=brewer.pal(n=8,name="RdBu")[7],
        boxlwd=2,whisklwd=2,staplelwd=2,cex=1.3,
        cex.main=2.5,cex.axis=2,cex.lab=2,main="Medium homogeneity",ylim=c(0,4.32),frame=T,yaxt="n")
axis(1,at=1:4,c("30","50","70","100"),line=0.6, lwd=0, cex.axis=2)
boxplot(fd_l530,fd_l550,fd_l570,fd_l5100,
        names=c("","","",""),col=brewer.pal(n=8,name="RdBu")[7],pch=19,outcol=brewer.pal(n=8,name="RdBu")[7],
        boxlwd=2,whisklwd=2,staplelwd=2,cex=1.3,
        cex.main=2.5,cex.axis=2,cex.lab=2,main="High homogeneity",ylim=c(0,4.32),frame=T,yaxt="n")
axis(1,at=1:4,c("30","50","70","100"),line=0.6, lwd=0, cex.axis=2)
mtext("Number of nodes, n", side = 1, line = 3, outer = TRUE, font=1, cex=1.6)
mtext(TeX("$|| \\textbf{A} - \\textbf{\\hat{A}} ||_F$"), side = 2, line = 1.3, outer = TRUE, font=1, cex=1.6)
#dev.off()

