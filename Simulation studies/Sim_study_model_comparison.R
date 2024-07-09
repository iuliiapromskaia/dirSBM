# Clustering performance simulation study: comparing binary SBM, Gaussian SBM on untransformed
# data, Gaussian SBM on log-ratio transformed data and Dirichlet SBM. Dirichlet SBM is initialsed 
# using 5 random starts. Number of clusters K is assumed to be known and is fixed

# Required packages
library(igraph)
library(blockmodels)
library(compositions)
library(mclust)
library(RColorBrewer)
library(kernlab)
library(doParallel)

# The results of this simulation study are available in sim_study_model_comparison.DRata
# dirSBM\Simualtion studies\Model_comparison_results
# Dataset names: simHKn, where H - parameter homogeneity (small(S)/medium(M)/large(L)),
#                              K - number of clusters (here, 2, 3 or 5)
#                              n - number of nodes (here, 30, 50, 70 or 100)

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
simS230=sim_study(K,n,Alpha)
#save(simS230,file="simS230.RData")
stopCluster(cl)

set.seed(256762) 
K=2
n=50
Alpha=matrix(c(1,0.6,0.8,1.5),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
simS250=sim_study(K,n,Alpha)
#save(simS250,file="simS250.RData")
stopCluster(cl)

set.seed(8756)
K=2
n=70
Alpha=matrix(c(1,0.6,0.8,1.5),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
simS270=sim_study(K,n,Alpha)
#save(simS270,file="simS270.RData")
stopCluster(cl)

set.seed(75358)
K=2
n=100
Alpha=matrix(c(1,0.6,0.8,1.5),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
simS2100=sim_study(K,n,Alpha)
#save(simS2100,file="simS2100.RData")
stopCluster(cl)

### Medium
set.seed(47837)
K=2
n=30
Alpha=matrix(c(1,0.6,0.9,1.4),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
simM230=sim_study(K,n,Alpha)
#save(simM230,file="simM230.RData")
stopCluster(cl)

set.seed(83011)
K=2
n=50
Alpha=matrix(c(1,0.6,0.9,1.4),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
simM250=sim_study(K,n,Alpha)
#save(simM250,file="simM250.RData")
stopCluster(cl)

set.seed(2754)
K=2
n=70
Alpha=matrix(c(1,0.6,0.9,1.4),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
simM270=sim_study(K,n,Alpha)
#save(simM270,file="simM270.RData")
stopCluster(cl)

set.seed(3573)
K=2
n=100
Alpha=matrix(c(1,0.6,0.9,1.4),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
simM2100=sim_study(K,n,Alpha)
#save(simM2100,file="simM2100.RData")
stopCluster(cl)

### Large
set.seed(84651)
K=2
n=30
Alpha=matrix(c(1,0.8,0.9,1.5),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
simL230=sim_study(K,n,Alpha)
#save(simL230,file="simL230.RData")
stopCluster(cl)

set.seed(884465)
K=2
n=50
Alpha=matrix(c(1,0.8,0.9,1.5),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
simL250=sim_study(K,n,Alpha)
#save(simL250,file="simL250.RData")
stopCluster(cl)

set.seed(10192)
K=2
n=70
Alpha=matrix(c(1,0.8,0.9,1.5),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
simL270=sim_study(K,n,Alpha)
#save(simL270,file="simL270.RData")
stopCluster(cl)

set.seed(99563)
K=2
n=100
Alpha=matrix(c(1,0.8,0.9,1.5),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
simL2100=sim_study(K,n,Alpha)
#save(simL2100,file="simL2100.RData")
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
simS330=sim_study(K,n,Alpha)
#save(simS330,file="simS330.RData")
stopCluster(cl)

set.seed(8589)
K=3
n=50
Alpha=matrix(c(1,0.6,0.2,0.6,1.5,0.5,0.3,0.4,1.2),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
simS350=sim_study(K,n,Alpha)
#save(simS350,file="simS350.RData")
stopCluster(cl)

set.seed(752)
K=3
n=70
Alpha=matrix(c(1,0.6,0.2,0.6,1.5,0.5,0.3,0.4,1.2),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
simS370=sim_study(K,n,Alpha)
#save(simS370,file="simS370.RData")
stopCluster(cl)

set.seed(354)
K=3
n=100
Alpha=matrix(c(1,0.6,0.2,0.6,1.5,0.5,0.3,0.4,1.2),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
simS3100=sim_study(K,n,Alpha)
#save(simS3100,file="simS3100.RData")
stopCluster(cl)

### Medium
set.seed(9876)
K=3
n=30
Alpha=matrix(c(1,0.7,0.5,0.9,1.5,0.6,0.4,0.5,1.2),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
simM330=sim_study(K,n,Alpha)
#save(simM330,file="simM330.RData")
stopCluster(cl)

set.seed(7451)
K=3
n=50
Alpha=matrix(c(1,0.7,0.5,0.9,1.5,0.6,0.4,0.5,1.2),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
simM350=sim_study(K,n,Alpha)
#save(simM350,file="simM350.RData")
stopCluster(cl)

set.seed(385)
K=3
n=70
Alpha=matrix(c(1,0.7,0.5,0.9,1.5,0.6,0.4,0.5,1.2),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
simM370=sim_study(K,n,Alpha)
#save(simM370,file="simM370.RData")
stopCluster(cl)

set.seed(1959)
K=3
n=100
Alpha=matrix(c(1,0.7,0.5,0.9,1.5,0.6,0.4,0.5,1.2),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
simM3100=sim_study(K,n,Alpha)
#save(simM3100,file="simM3100.RData")
stopCluster(cl)

### Large
set.seed(546576)
K=3
n=30
Alpha=matrix(c(1,0.7,0.5,0.9,1.3,0.7,0.6,0.5,1.2),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
simL330=sim_study(K,n,Alpha)
#save(simL330,file="simL330.RData")
stopCluster(cl)

set.seed(836) 
K=3
n=50
Alpha=matrix(c(1,0.7,0.5,0.9,1.3,0.7,0.6,0.5,1.2),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
simL350=sim_study(K,n,Alpha)
#save(simL350,file="simL350.RData")
stopCluster(cl)

set.seed(76) 
K=3
n=70
Alpha=matrix(c(1,0.7,0.5,0.9,1.3,0.7,0.6,0.5,1.2),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
simL370=sim_study(K,n,Alpha)
#save(simL370,file="simL370.RData")
stopCluster(cl)

set.seed(293)
K=3
n=100
Alpha=matrix(c(1,0.7,0.5,0.9,1.3,0.7,0.6,0.5,1.2),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
simL3100=sim_study(K,n,Alpha)
#save(simL3100,file="simL3100.RData")
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
simS530=sim_study(K,n,Alpha)
#save(simS530,file="simS530.RData")
stopCluster(cl)

set.seed(7654)
K=5
n=50
Alpha=matrix(c(1,0.6,0.2,0.3,0.5,0.6,1.5,0.5,0.4,0.7,0.3,0.4,1.2,0.5,0.2,0.7,0.5,
               0.3,1.4,0.4,0.5,0.7,0.8,0.6,1.7),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
simS550=sim_study(K,n,Alpha)
#save(simS550,file="simS550.RData")
stopCluster(cl)

set.seed(836012)
K=5
n=70
Alpha=matrix(c(1,0.6,0.2,0.3,0.5,0.6,1.5,0.5,0.4,0.7,0.3,0.4,1.2,0.5,0.2,0.7,0.5,
               0.3,1.4,0.4,0.5,0.7,0.8,0.6,1.7),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
simS570=sim_study(K,n,Alpha)
#save(simS570,file="simS570.RData")
stopCluster(cl)

set.seed(9815003)
K=5
n=100
Alpha=matrix(c(1,0.6,0.2,0.3,0.5,0.6,1.5,0.5,0.4,0.7,0.3,0.4,1.2,0.5,0.2,0.7,0.5,
               0.3,1.4,0.4,0.5,0.7,0.8,0.6,1.7),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
simS5100=sim_study(K,n,Alpha)
#save(simS5100,file="simS5100.RData")
stopCluster(cl)

### Medium
set.seed(18234678)
K=5
n=30
Alpha=matrix(c(1,0.7,0.5,0.4,0.6,0.9,1.5,0.6,0.5,0.7,0.4,0.5,1.2,0.6,0.3,0.8,0.6,
               0.4,1.4,0.5,0.5,0.8,0.9,0.7,1.7),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
simM530=sim_study(K,n,Alpha)
#save(simM530,file="simM530.RData")
stopCluster(cl)

set.seed(9070008)
K=5
n=50
Alpha=matrix(c(1,0.7,0.5,0.4,0.6,0.9,1.5,0.6,0.5,0.7,0.4,0.5,1.2,0.6,0.3,0.8,0.6,
               0.4,1.4,0.5,0.5,0.8,0.9,0.7,1.7),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
simM550=sim_study(K,n,Alpha)
#save(simM550,file="simM550.RData")
stopCluster(cl)

set.seed(8940111)
K=5
n=70
Alpha=matrix(c(1,0.7,0.5,0.4,0.6,0.9,1.5,0.6,0.5,0.7,0.4,0.5,1.2,0.6,0.3,0.8,0.6,
               0.4,1.4,0.5,0.5,0.8,0.9,0.7,1.7),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
simM570=sim_study(K,n,Alpha)
#save(simM570,file="simM570.RData")
stopCluster(cl)

set.seed(7774456) 
K=5
n=100
Alpha=matrix(c(1,0.7,0.5,0.4,0.6,0.9,1.5,0.6,0.5,0.7,0.4,0.5,1.2,0.6,0.3,0.8,0.6,
               0.4,1.4,0.5,0.5,0.8,0.9,0.7,1.7),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
simM5100=sim_study(K,n,Alpha)
#save(simM5100,file="simM5100.RData")
stopCluster(cl)

### Large
set.seed(54634)
K=5
n=30
Alpha=matrix(c(1.0,0.7,0.5,0.4,0.6,0.9,1.3,0.7,0.5,0.8,0.6,0.7,1.2,0.8,0.5,0.8,0.6,
               0.4,1.4,0.7,0.7,0.8,0.9,0.6,1.6),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
simL530=sim_study(K,n,Alpha)
#save(simL530,file="simL530.RData")
stopCluster(cl)

set.seed(7000503)
K=5
n=50
Alpha=matrix(c(1.0,0.7,0.5,0.4,0.6,0.9,1.3,0.7,0.5,0.8,0.6,0.7,1.2,0.8,0.5,0.8,0.6,
               0.4,1.4,0.7,0.7,0.8,0.9,0.6,1.6),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
simL550=sim_study(K,n,Alpha)
#save(simL550,file="simL550.RData")
stopCluster(cl)

set.seed(199957)
K=5
n=70
Alpha=matrix(c(1.0,0.7,0.5,0.4,0.6,0.9,1.3,0.7,0.5,0.8,0.6,0.7,1.2,0.8,0.5,0.8,0.6,
               0.4,1.4,0.7,0.7,0.8,0.9,0.6,1.6),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
simL570=sim_study(K,n,Alpha)
#save(simL570,file="simL570.RData")
stopCluster(cl)

set.seed(4442217)
K=5
n=100
Alpha=matrix(c(1.0,0.7,0.5,0.4,0.6,0.9,1.3,0.7,0.5,0.8,0.6,0.7,1.2,0.8,0.5,0.8,0.6,
               0.4,1.4,0.7,0.7,0.8,0.9,0.6,1.6),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
simL5100=sim_study(K,n,Alpha)
#save(simL5100,file="simL5100.RData")
stopCluster(cl)





################################### Plots

##### ALL RESULT FOR K=2
#pdf("sim_study_boxplots_K=2.pdf",width=7.6*1.925,height=7.6)
par(mfrow=c(3,5),oma = c(1,4,2.5,0.1) + 0.1,mar = c(0,2,1.5,0))

boxplot(simS230$sbm_ARand,simS230$g_sbm_naive_ARand,simS230$g_sbm_ARand,simS230$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),pch=19,outcol=brewer.pal(n=4,name="Set1"),
        boxlwd=2,whisklwd=2,staplelwd=2,cex=1.3,cex.main=2.3,cex.axis=1.8,cex.lab=2,main="",ylim=c(0,1),frame=T,xaxt="n")
mtext("n=30                     n=50                      n=70                      n=100                         ",
      side = 3, line = 0, outer = TRUE, font=2, cex=2)
boxplot(simS250$sbm_ARand,simS250$g_sbm_naive_ARand,simS250$g_sbm_ARand,simS250$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),outcol=brewer.pal(n=4,name="Set1"),pch=19,boxlwd=2,
        whisklwd=2,staplelwd=2,cex=1.3,cex.main=2.3,cex.axis=2,cex.lab=2,main="",ylim=c(0.2,1),axes=F,frame=T)
boxplot(simS270$sbm_ARand,simS270$g_sbm_naive_ARand,simS270$g_sbm_ARand,simS270$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),pch=19,outcol=brewer.pal(n=4,name="Set1"),boxlwd=2,
        whisklwd=2,staplelwd=2,cex=1.3,cex.main=2.3,cex.axis=2,cex.lab=2,main="",ylim=c(0.2,1),axes=F,frame=T)
boxplot(simS2100$sbm_ARand,simS2100$g_sbm_naive_ARand,simS2100$g_sbm_ARand,simS2100$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),outcol=brewer.pal(n=4,name="Set1"),pch=19,boxlwd=2,
        whisklwd=2,staplelwd=2,cex=1.3,cex.main=2.3,cex.axis=2,cex.lab=2,main="",ylim=c(0.2,1),axes=F,frame=T)
plot.new()
legend("topleft",legend=c("BinSBM","GausSBM","CLR+GausSBM","DirSBM"),fill=brewer.pal(n=4,name="Set1"),box.col="black",
       cex=2.3,border=brewer.pal(n=4,name="Set1"))
text(x=0.5,y=0.13," Low homogeneity",cex=2.3)
mtext("Adjusted Rand Index", side = 2, line = 2, outer = TRUE, cex=1.75)

boxplot(simM230$sbm_ARand,simM230$g_sbm_naive_ARand,simM230$g_sbm_ARand,simM230$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),pch=19,outcol=brewer.pal(n=4,name="Set1"),boxlwd=2,
        whisklwd=2,staplelwd=2,cex=1.3,cex.main=2.5,cex.axis=1.8,cex.lab=2,main="",ylim=c(0,1),frame=T,xaxt="n")
boxplot(simM250$sbm_ARand,simM250$g_sbm_naive_ARand,simM250$g_sbm_ARand,simM250$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),outcol=brewer.pal(n=4,name="Set1"),pch=19,boxlwd=2,
        whisklwd=2,staplelwd=2,cex=1.3,cex.main=2.5,cex.axis=2,cex.lab=2,main="",ylim=c(0,1),axes=F,frame=T)
boxplot(simM270$sbm_ARand,simM270$g_sbm_naive_ARand,simM270$g_sbm_ARand,simM270$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),pch=19,outcol=brewer.pal(n=4,name="Set1"),boxlwd=2,
        whisklwd=2,staplelwd=2,cex=1.3,cex.main=2.5,cex.axis=2,cex.lab=2,main="",ylim=c(0,1),axes=F,frame=T)
boxplot(simM2100$sbm_ARand,simM2100$g_sbm_naive_ARand,simM2100$g_sbm_ARand,simM2100$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),outcol=brewer.pal(n=4,name="Set1"),pch=19,boxlwd=2,
        whisklwd=2,staplelwd=2,cex=1.3,cex.main=2.5,cex.axis=2,cex.lab=2,main="",ylim=c(0,1),axes=F,frame=T)
plot.new()
text(x=0.5,y=0.5," Medium homogeneity",cex=2.3)

boxplot(simL230$sbm_ARand,simL230$g_sbm_naive_ARand,simL230$g_sbm_ARand,simL230$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),pch=19,outcol=brewer.pal(n=4,name="Set1"),boxlwd=2,
        whisklwd=2,staplelwd=2,cex=1.3,cex.main=2.5,cex.axis=1.8,cex.lab=2,main="",ylim=c(0,1),frame=T,xaxt="n")
boxplot(simL250$sbm_ARand,simL250$g_sbm_naive_ARand,simL250$g_sbm_ARand,simL250$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),outcol=brewer.pal(n=4,name="Set1"),pch=19,boxlwd=2,
        whisklwd=2,staplelwd=2,cex=1.3,cex.main=2.5,cex.axis=2,cex.lab=2,main="",ylim=c(0,1),axes=F,frame=T)
boxplot(simL270$sbm_ARand,simL270$g_sbm_naive_ARand,simL270$g_sbm_ARand,simL270$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),pch=19,outcol=brewer.pal(n=4,name="Set1"),boxlwd=2,
        whisklwd=2,staplelwd=2,cex=1.3,cex.main=2.5,cex.axis=2,cex.lab=2,main="",ylim=c(0,1),axes=F,frame=T)
boxplot(simL2100$sbm_ARand,simL2100$g_sbm_naive_ARand,simL2100$g_sbm_ARand,simL2100$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),outcol=brewer.pal(n=4,name="Set1"),pch=19,boxlwd=2,
        whisklwd=2,staplelwd=2,cex=1.3,cex.main=2.5,cex.axis=2,cex.lab=2,main="",ylim=c(0,1),axes=F,frame=T)
plot.new()
text(x=0.5,y=0.5," High homogeneity",cex=2.3)
#dev.off()


##### ALL RESULT FOR K=3
#pdf("sim_study_boxplots_K=3.pdf",width=7.6*1.925,height=7.6)
par(mfrow=c(3,5),oma = c(1,4,2.5,0.1) + 0.1,mar = c(0,2,1.5,0))

boxplot(simS330$sbm_ARand,simS330$g_sbm_naive_ARand,simS330$g_sbm_ARand,simS330$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),pch=19,outcol=brewer.pal(n=4,name="Set1"),
        boxlwd=2,whisklwd=2,staplelwd=2,cex=1.3,cex.main=2.3,cex.axis=1.8,cex.lab=2,main="",ylim=c(0,1),frame=T,xaxt="n")
mtext("n=30                     n=50                      n=70                      n=100                         ",
      side = 3, line = 0, outer = TRUE, font=2, cex=2)
boxplot(simS350$sbm_ARand,simS350$g_sbm_naive_ARand,simS350$g_sbm_ARand,simS350$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),outcol=brewer.pal(n=4,name="Set1"),pch=19,boxlwd=2,
        whisklwd=2,staplelwd=2,cex=1.3,cex.main=2.3,cex.axis=2,cex.lab=2,main="",ylim=c(0.2,1),axes=F,frame=T)
boxplot(simS370$sbm_ARand,simS370$g_sbm_naive_ARand,simS370$g_sbm_ARand,simS370$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),pch=19,outcol=brewer.pal(n=4,name="Set1"),boxlwd=2,
        whisklwd=2,staplelwd=2,cex=1.3,cex.main=2.3,cex.axis=2,cex.lab=2,main="",ylim=c(0.2,1),axes=F,frame=T)
boxplot(simS3100$sbm_ARand,simS3100$g_sbm_naive_ARand,simS3100$g_sbm_ARand,simS3100$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),outcol=brewer.pal(n=4,name="Set1"),pch=19,boxlwd=2,
        whisklwd=2,staplelwd=2,cex=1.3,cex.main=2.3,cex.axis=2,cex.lab=2,main="",ylim=c(0.2,1),axes=F,frame=T)
plot.new()
legend("topleft",legend=c("BinSBM","GausSBM","CLR+GausSBM","DirSBM"),fill=brewer.pal(n=4,name="Set1"),box.col="black",
       cex=2.3,border=brewer.pal(n=4,name="Set1"))
text(x=0.5,y=0.13," Low homogeneity",cex=2.3)
mtext("Adjusted Rand Index", side = 2, line = 2, outer = TRUE, cex=1.75)

boxplot(simM330$sbm_ARand,simM330$g_sbm_naive_ARand,simM330$g_sbm_ARand,simM330$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),pch=19,outcol=brewer.pal(n=4,name="Set1"),
        boxlwd=2,whisklwd=2,staplelwd=2,cex=1.3,
        cex.main=2.5,cex.axis=1.8,cex.lab=2,main="",ylim=c(0,1),frame=T,xaxt="n")
boxplot(simM350$sbm_ARand,simM350$g_sbm_naive_ARand,simM350$g_sbm_ARand,simM350$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),outcol=brewer.pal(n=4,name="Set1"),pch=19,boxlwd=2,
        whisklwd=2,staplelwd=2,cex=1.3,cex.main=2.5,cex.axis=2,cex.lab=2,main="",ylim=c(0,1),axes=F,frame=T)
boxplot(simM370$sbm_ARand,simM370$g_sbm_naive_ARand,simM370$g_sbm_ARand,simM370$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),pch=19,outcol=brewer.pal(n=4,name="Set1"),boxlwd=2,
        whisklwd=2,staplelwd=2,cex=1.3,cex.main=2.5,cex.axis=2,cex.lab=2,main="",ylim=c(0,1),axes=F,frame=T)
boxplot(simM3100$sbm_ARand,simM3100$g_sbm_naive_ARand,simM3100$g_sbm_ARand,simM3100$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),outcol=brewer.pal(n=4,name="Set1"),pch=19,boxlwd=2,
        whisklwd=2,staplelwd=2,cex=1.3,cex.main=2.5,cex.axis=2,cex.lab=2,main="",ylim=c(0,1),axes=F,frame=T)
plot.new()
text(x=0.5,y=0.5," Medium homogeneity",cex=2.3)

boxplot(simL330$sbm_ARand,simL330$g_sbm_naive_ARand,simL330$g_sbm_ARand,simL330$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),pch=19,outcol=brewer.pal(n=4,name="Set1"),
        boxlwd=2,whisklwd=2,staplelwd=2,cex=1.3,cex.main=2.5,cex.axis=1.8,cex.lab=2,main="",ylim=c(0,1),frame=T,xaxt="n")
boxplot(simL350$sbm_ARand,simL350$g_sbm_naive_ARand,simL350$g_sbm_ARand,simL350$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),outcol=brewer.pal(n=4,name="Set1"),pch=19,boxlwd=2,
        whisklwd=2,staplelwd=2,cex=1.3,cex.main=2.5,cex.axis=2,cex.lab=2,main="",ylim=c(0,1),axes=F,frame=T)
boxplot(simL370$sbm_ARand,simL370$g_sbm_naive_ARand,simL370$g_sbm_ARand,simL370$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),pch=19,outcol=brewer.pal(n=4,name="Set1"),boxlwd=2,
        whisklwd=2,staplelwd=2,cex=1.3,cex.main=2.5,cex.axis=2,cex.lab=2,main="",ylim=c(0,1),axes=F,frame=T)
boxplot(simL3100$sbm_ARand,simL3100$g_sbm_naive_ARand,simL3100$g_sbm_ARand,simL3100$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),outcol=brewer.pal(n=4,name="Set1"),pch=19,boxlwd=2,
        whisklwd=2,staplelwd=2,cex=1.3,cex.main=2.5,cex.axis=2,cex.lab=2,main="",ylim=c(0,1),axes=F,frame=T)
plot.new()
text(x=0.5,y=0.5," High homogeneity",cex=2.3)
#dev.off()


##### ALL RESULT FOR K=5
#pdf("sim_study_boxplots_K=5.pdf",width=7.6*1.925,height=7.6)
par(mfrow=c(3,5),oma = c(1,4,2.5,0.1) + 0.1,mar = c(0,2,1.5,0))

boxplot(simS530$sbm_ARand,simS530$g_sbm_naive_ARand,simS530$g_sbm_ARand,simS530$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),pch=19,outcol=brewer.pal(n=4,name="Set1"),
        boxlwd=2,whisklwd=2,staplelwd=2,cex=1.3,
        cex.main=2.3,cex.axis=1.8,cex.lab=2,main="",ylim=c(0,1),frame=T,xaxt="n")
mtext("n=30                     n=50                      n=70                      n=100                         ",
      side = 3, line = 0, outer = TRUE, font=2, cex=2)
boxplot(simS550$sbm_ARand,simS550$g_sbm_naive_ARand,simS550$g_sbm_ARand,simS550$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),outcol=brewer.pal(n=4,name="Set1"),pch=19,boxlwd=2,
        whisklwd=2,staplelwd=2,cex=1.3,cex.main=2.3,cex.axis=2,cex.lab=2,main="",ylim=c(0.2,1),axes=F,frame=T)
boxplot(simS570$sbm_ARand,simS570$g_sbm_naive_ARand,simS570$g_sbm_ARand,simS570$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),pch=19,outcol=brewer.pal(n=4,name="Set1"),boxlwd=2,
        whisklwd=2,staplelwd=2,cex=1.3,cex.main=2.3,cex.axis=2,cex.lab=2,main="",ylim=c(0.2,1),axes=F,frame=T)
boxplot(simS5100$sbm_ARand,simS5100$g_sbm_naive_ARand,simS5100$g_sbm_ARand,simS5100$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),outcol=brewer.pal(n=4,name="Set1"),pch=19,boxlwd=2,
        whisklwd=2,staplelwd=2,cex=1.3,cex.main=2.3,cex.axis=2,cex.lab=2,main="",ylim=c(0.2,1),axes=F,frame=T)
plot.new()
legend("topleft",legend=c("BinSBM","GausSBM","CLR+GausSBM","DirSBM"),fill=brewer.pal(n=4,name="Set1"),box.col="black",
       cex=2.3,border=brewer.pal(n=4,name="Set1"))
text(x=0.5,y=0.13," Low homogeneity",cex=2.3)
mtext("Adjusted Rand Index", side = 2, line = 2, outer = TRUE, cex=1.75)

boxplot(simM530$sbm_ARand,simM530$g_sbm_naive_ARand,simM530$g_sbm_ARand,simM530$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),pch=19,outcol=brewer.pal(n=4,name="Set1"),
        boxlwd=2,whisklwd=2,staplelwd=2,cex=1.3,cex.main=2.5,cex.axis=1.8,cex.lab=2,main="",ylim=c(0,1),frame=T,xaxt="n")
boxplot(simM550$sbm_ARand,simM550$g_sbm_naive_ARand,simM550$g_sbm_ARand,simM550$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),outcol=brewer.pal(n=4,name="Set1"),pch=19,boxlwd=2,
        whisklwd=2,staplelwd=2,cex=1.3,cex.main=2.5,cex.axis=2,cex.lab=2,main="",ylim=c(0,1),axes=F,frame=T)
boxplot(simM570$sbm_ARand,simM570$g_sbm_naive_ARand,simM570$g_sbm_ARand,simM570$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),pch=19,outcol=brewer.pal(n=4,name="Set1"),boxlwd=2,
        whisklwd=2,staplelwd=2,cex=1.3,cex.main=2.5,cex.axis=2,cex.lab=2,main="",ylim=c(0,1),axes=F,frame=T)
boxplot(simM5100$sbm_ARand,simM5100$g_sbm_naive_ARand,simM5100$g_sbm_ARand,simM5100$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),outcol=brewer.pal(n=4,name="Set1"),pch=19,boxlwd=2,
        whisklwd=2,staplelwd=2,cex=1.3,cex.main=2.5,cex.axis=2,cex.lab=2,main="",ylim=c(0,1),axes=F,frame=T)
plot.new()
text(x=0.5,y=0.5," Medium homogeneity",cex=2.3)

boxplot(simL530$sbm_ARand,simL530$g_sbm_naive_ARand,simL530$g_sbm_ARand,simL530$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),pch=19,outcol=brewer.pal(n=4,name="Set1"),
        boxlwd=2,whisklwd=2,staplelwd=2,cex=1.3,cex.main=2.5,cex.axis=1.8,cex.lab=2,main="",ylim=c(0,1),frame=T,xaxt="n")
boxplot(simL550$sbm_ARand,simL550$g_sbm_naive_ARand,simL550$g_sbm_ARand,simL550$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),outcol=brewer.pal(n=4,name="Set1"),pch=19,boxlwd=2,
        whisklwd=2,staplelwd=2,cex=1.3,cex.main=2.5,cex.axis=2,cex.lab=2,main="",ylim=c(0,1),axes=F,frame=T)
boxplot(simL570$sbm_ARand,simL570$g_sbm_naive_ARand,simL570$g_sbm_ARand,simL570$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),pch=19,outcol=brewer.pal(n=4,name="Set1"),boxlwd=2,
        whisklwd=2,staplelwd=2,cex=1.3,cex.main=2.5,cex.axis=2,cex.lab=2,main="",ylim=c(0,1),axes=F,frame=T)
boxplot(simL5100$sbm_ARand,simL5100$g_sbm_naive_ARand,simL5100$g_sbm_ARand,simL5100$dir_sbm_ARand,
        names=c("","","",""),col=brewer.pal(n=4,name="Set1"),outcol=brewer.pal(n=4,name="Set1"),pch=19,boxlwd=2,
        whisklwd=2,staplelwd=2,cex=1.3,cex.main=2.5,cex.axis=2,cex.lab=2,main="",ylim=c(0,1),axes=F,frame=T)
plot.new()
text(x=0.5,y=0.5," High homogeneity",cex=2.3)
#dev.off()

