# Model selection simulation study: assessing model selection performance of Integrated Completed
# Likelihood (ICL) for Dirichlet SBM. Generating 50 synthetic data sets and fitting the models
# with k=1,...,K_max clusters. Optimal k has the highest icl_vals.

# Required packages
library(doParallel)
library(igraph)
library(LaplacesDemon)
library(igraph)

# The results of this simulation study are available in dirSBM\Simualtion studies\Model_selection_results
# Dataset names: HKn, where H - parameter homogeneity (small(S)/medium(M)/large(L)),
#                           K - number of clusters (here, 2, 3 or 5)
#                           n - number of nodes (here, 30, 50, 70 or 100)



################
##### K=2 ######
################

### Small
set.seed(3658)
K=2
n=30
Alpha=matrix(c(1,0.6,0.8,1.5),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
S230=K_selection_study(n,K,Alpha,4)
stopCluster(cl)
#save(S230,file="S230.RData")
table(apply(S230$icl_vals,1,which.max))

set.seed(3757)
K=2
n=50
Alpha=matrix(c(1,0.6,0.8,1.5),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
S250=K_selection_study(n,K,Alpha,4)
stopCluster(cl)
#save(S250,file="S250.RData")
table(apply(S250$icl_vals,1,which.max))

set.seed(8756)
K=2
n=70
Alpha=matrix(c(1,0.6,0.8,1.5),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
S270=K_selection_study(n,K,Alpha,4)
stopCluster(cl)
#save(S270,file="S270.RData")
table(apply(S270$icl_vals,1,which.max))

set.seed(56872)
K=2
n=100
Alpha=matrix(c(1,0.6,0.8,1.5),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
S2100=K_selection_study(n,K,Alpha,4)
stopCluster(cl)
#save(S2100,file="S2100.RData")
table(apply(S2100$icl_vals,1,which.max))


### Medium
set.seed(8765)
K=2
n=30
Alpha=matrix(c(1,0.6,0.9,1.4),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
M230=K_selection_study(n,K,Alpha,4)
stopCluster(cl)
#save(M230,file="M230.RData")
table(apply(M230$icl_vals,1,which.max))

set.seed(745242)
K=2
n=50
Alpha=matrix(c(1,0.6,0.9,1.4),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
M250=K_selection_study(n,K,Alpha,4)
stopCluster(cl)
#save(M250,file="M250.RData")
table(apply(M250$icl_vals,1,which.max))

set.seed(2754)
K=2
n=70
Alpha=matrix(c(1,0.6,0.9,1.4),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
M270=K_selection_study(n,K,Alpha,4)
stopCluster(cl)
#save(M270,file="M270.RData")
table(apply(M270$icl_vals,1,which.max))

set.seed(193843)
K=2
n=100
Alpha=matrix(c(1,0.6,0.9,1.4),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
M2100=K_selection_study(n,K,Alpha,4)
stopCluster(cl)
#save(M2100,file="M2100.RData")
table(apply(M2100$icl_vals,1,which.max))


### Large
set.seed(7478)
K=2
n=30
Alpha=matrix(c(1,0.8,0.9,1.5),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
L230=K_selection_study(n,K,Alpha,4)
stopCluster(cl)
#save(L230,file="L230.RData")
table(apply(L230$icl_vals,1,which.max))

set.seed(53487)
K=2
n=50
Alpha=matrix(c(1,0.8,0.9,1.5),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
L250=K_selection_study(n,K,Alpha,4)
stopCluster(cl)
#save(L250,file="L250.RData")
table(apply(L250$icl_vals,1,which.max))

set.seed(5767)
K=2
n=70
Alpha=matrix(c(1,0.8,0.9,1.5),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
L270=K_selection_study(n,K,Alpha,4)
stopCluster(cl)
#save(L270,file="L270.RData")
table(apply(L270$icl_vals,1,which.max))

set.seed(86)
K=2
n=100
Alpha=matrix(c(1,0.8,0.9,1.5),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
L2100=K_selection_study(n,K,Alpha,4)
stopCluster(cl)
#save(L2100,file="L2100.RData")
table(apply(L2100$icl_vals,1,which.max))


################
##### K=3 ######
################

### Small
set.seed(5462)
K=3
n=30
Alpha=matrix(c(1,0.6,0.2,0.6,1.5,0.5,0.3,0.4,1.2),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
S330=K_selection_study(n,K,Alpha,4)
stopCluster(cl)
#save(S330,file="S330.RData")
table(apply(S330$icl_vals,1,which.max))

set.seed(8356)
K=3
n=50
Alpha=matrix(c(1,0.6,0.2,0.6,1.5,0.5,0.3,0.4,1.2),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
S350=K_selection_study(n,K,Alpha,4)
stopCluster(cl)
#save(S350,file="S350.RData")
table(apply(S350$icl_vals,1,which.max))

set.seed(987689)
K=3
n=70
Alpha=matrix(c(1,0.6,0.2,0.6,1.5,0.5,0.3,0.4,1.2),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
S370=K_selection_study(n,K,Alpha,4)
stopCluster(cl)
#save(S370,file="S370.RData")
table(apply(S370$icl_vals,1,which.max))

set.seed(3155413)
K=3
n=100
Alpha=matrix(c(1,0.6,0.2,0.6,1.5,0.5,0.3,0.4,1.2),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
S3100=K_selection_study(n,K,Alpha,4)
stopCluster(cl)
#save(S3100,file="S3100.RData")
table(apply(S3100$icl_vals,1,which.max))


### Medium
set.seed(9898)
K=3
n=30
Alpha=matrix(c(1,0.7,0.5,0.9,1.5,0.6,0.4,0.5,1.2),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
M330=K_selection_study(n,K,Alpha,4)
stopCluster(cl)
#save(M330,file="M330.RData")
table(apply(M330$icl_vals,1,which.max))

set.seed(34535)
K=3
n=50
Alpha=matrix(c(1,0.7,0.5,0.9,1.5,0.6,0.4,0.5,1.2),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
M350=K_selection_study(n,K,Alpha,4)
stopCluster(cl)
#save(M350,file="M350.RData")
table(apply(M350$icl_vals,1,which.max))

set.seed(89895)
K=3
n=70
Alpha=matrix(c(1,0.7,0.5,0.9,1.5,0.6,0.4,0.5,1.2),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
M370=K_selection_study(n,K,Alpha,4)
stopCluster(cl)
#save(M370,file="M370.RData")
table(apply(M370$icl_vals,1,which.max))

set.seed(9065)
K=3
n=100
Alpha=matrix(c(1,0.7,0.5,0.9,1.5,0.6,0.4,0.5,1.2),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
M3100=K_selection_study(n,K,Alpha,4)
stopCluster(cl)
#save(M3100,file="M3100.RData")
table(apply(M3100$icl_vals,1,which.max))


### Large
set.seed(43433)
K=3
n=30
Alpha=matrix(c(1,0.7,0.5,0.9,1.3,0.7,0.6,0.5,1.2),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
L330=K_selection_study(n,K,Alpha,4)
stopCluster(cl)
#save(L330,file="L330.RData")
table(apply(L330$icl_vals,1,which.max))

set.seed(5657)
K=3
n=50
Alpha=matrix(c(1,0.7,0.5,0.9,1.3,0.7,0.6,0.5,1.2),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
L350=K_selection_study(n,K,Alpha,4)
stopCluster(cl)
#save(L350,file="L350.RData")
table(apply(L350$icl_vals,1,which.max))

set.seed(7648)
K=3
n=70
Alpha=matrix(c(1,0.7,0.5,0.9,1.3,0.7,0.6,0.5,1.2),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
L370=K_selection_study(n,K,Alpha,4)
stopCluster(cl)
#save(L370,file="L370.RData")
table(apply(L370$icl_vals,1,which.max))

set.seed(76560)
K=3
n=100
Alpha=matrix(c(1,0.7,0.5,0.9,1.3,0.7,0.6,0.5,1.2),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
L3100=K_selection_study(n,K,Alpha,4)
stopCluster(cl)
#save(L3100,file="L3100.RData")
table(apply(L3100$icl_vals,1,which.max))


################
##### K=5 ######
################

### Small
set.seed(828282)
K=5
n=30
Alpha=matrix(c(1,0.6,0.2,0.3,0.5,0.6,1.5,0.5,0.4,0.7,0.3,0.4,1.2,0.5,0.2,0.7,0.5,
               0.3,1.4,0.4,0.5,0.7,0.8,0.6,1.7),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
S530=K_selection_study(n,K,Alpha,6)
stopCluster(cl)
#save(S530,file="S530.RData")
table(apply(S530$icl_vals,1,which.max))

set.seed(123123)
K=5
n=50
Alpha=matrix(c(1,0.6,0.2,0.3,0.5,0.6,1.5,0.5,0.4,0.7,0.3,0.4,1.2,0.5,0.2,0.7,0.5,
               0.3,1.4,0.4,0.5,0.7,0.8,0.6,1.7),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
S550=K_selection_study(n,K,Alpha,6)
stopCluster(cl)
#save(S550,file="S550.RData")
table(apply(S550$icl_vals,1,which.max))

set.seed(9098)
K=5
n=70
Alpha=matrix(c(1,0.6,0.2,0.3,0.5,0.6,1.5,0.5,0.4,0.7,0.3,0.4,1.2,0.5,0.2,0.7,0.5,
               0.3,1.4,0.4,0.5,0.7,0.8,0.6,1.7),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
S570=K_selection_study(n,K,Alpha,6)
stopCluster(cl)
#save(S570,file="S570.RData")
table(apply(S570$icl_vals,1,which.max))

set.seed(238787)
K=5
n=100
Alpha=matrix(c(1,0.6,0.2,0.3,0.5,0.6,1.5,0.5,0.4,0.7,0.3,0.4,1.2,0.5,0.2,0.7,0.5,
               0.3,1.4,0.4,0.5,0.7,0.8,0.6,1.7),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
S5100=K_selection_study(n,K,Alpha,6)
stopCluster(cl)
#save(S5100,file="S5100.RData")
table(apply(S5100$icl_vals,1,which.max))


### Medium
set.seed(90908)
K=5
n=30
Alpha=matrix(c(1,0.7,0.5,0.4,0.6,0.9,1.5,0.6,0.5,0.7,0.4,0.5,1.2,0.6,0.3,0.8,0.6,
               0.4,1.4,0.5,0.5,0.8,0.9,0.7,1.7),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
M530=K_selection_study(n,K,Alpha,6)
stopCluster(cl)
#save(M530,file="M530.RData")
table(apply(M530$icl_vals,1,which.max))

set.seed(4474)
K=5
n=50
Alpha=matrix(c(1,0.7,0.5,0.4,0.6,0.9,1.5,0.6,0.5,0.7,0.4,0.5,1.2,0.6,0.3,0.8,0.6,
               0.4,1.4,0.5,0.5,0.8,0.9,0.7,1.7),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
M550=K_selection_study(n,K,Alpha,6)
stopCluster(cl)
#save(M550,file="M550.RData")
table(apply(M550$icl_vals,1,which.max))

set.seed(97778)
K=5
n=70
Alpha=matrix(c(1,0.7,0.5,0.4,0.6,0.9,1.5,0.6,0.5,0.7,0.4,0.5,1.2,0.6,0.3,0.8,0.6,
               0.4,1.4,0.5,0.5,0.8,0.9,0.7,1.7),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
M570=K_selection_study(n,K,Alpha,6)
stopCluster(cl)
#save(M570,file="M570.RData")
table(apply(M570$icl_vals,1,which.max))

set.seed(56542)
K=5
n=100
Alpha=matrix(c(1,0.7,0.5,0.4,0.6,0.9,1.5,0.6,0.5,0.7,0.4,0.5,1.2,0.6,0.3,0.8,0.6,
               0.4,1.4,0.5,0.5,0.8,0.9,0.7,1.7),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
M5100=K_selection_study(n,K,Alpha,6)
stopCluster(cl)
#save(M5100,file="M5100.RData")
table(apply(M5100$icl_vals,1,which.max))


### Large
set.seed(57963)
K=5
n=30
Alpha=matrix(c(1.0,0.7,0.5,0.4,0.6,0.9,1.3,0.7,0.5,0.8,0.6,0.7,1.2,0.8,0.5,0.8,0.6,
               0.4,1.4,0.7,0.7,0.8,0.9,0.6,1.6),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
L530=K_selection_study(n,K,Alpha,6)
stopCluster(cl)
#save(L530,file="L530.RData")
table(apply(L530$icl_vals,1,which.max))

set.seed(3434)
K=5
n=50
Alpha=matrix(c(1.0,0.7,0.5,0.4,0.6,0.9,1.3,0.7,0.5,0.8,0.6,0.7,1.2,0.8,0.5,0.8,0.6,
               0.4,1.4,0.7,0.7,0.8,0.9,0.6,1.6),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
L550=K_selection_study(n,K,Alpha,6)
stopCluster(cl)
#save(L550,file="L550.RData")
table(apply(L550$icl_vals,1,which.max))

set.seed(874573)
K=5
n=70
Alpha=matrix(c(1.0,0.7,0.5,0.4,0.6,0.9,1.3,0.7,0.5,0.8,0.6,0.7,1.2,0.8,0.5,0.8,0.6,
               0.4,1.4,0.7,0.7,0.8,0.9,0.6,1.6),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
L570=K_selection_study(n,K,Alpha,6)
stopCluster(cl)
#save(L570,file="L570.RData")
table(apply(L570$icl_vals,1,which.max))

set.seed(11134443)
K=5
n=100
Alpha=matrix(c(1.0,0.7,0.5,0.4,0.6,0.9,1.3,0.7,0.5,0.8,0.6,0.7,1.2,0.8,0.5,0.8,0.6,
               0.4,1.4,0.7,0.7,0.8,0.9,0.6,1.6),nrow=K,ncol=K,byrow=TRUE)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
L5100=K_selection_study(n,K,Alpha,6)
stopCluster(cl)
#save(L5100,file="L5100.RData")
table(apply(L5100$icl_vals,1,which.max))






###########################
# L550 was broken down into 5 parts and combined into a single data set at the end
K=5
n=50
Alpha=matrix(c(1.0,0.7,0.5,0.4,0.6,0.9,1.3,0.7,0.5,0.8,0.6,0.7,1.2,0.8,0.5,0.8,0.6,
               0.4,1.4,0.7,0.7,0.8,0.9,0.6,1.6),nrow=K,ncol=K,byrow=TRUE)

set.seed(3434)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
L550a=K_selection_study(n,K,Alpha,6)
stopCluster(cl)
#save(L550a,file="L550a.RData")

set.seed(5738)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
L550b=K_selection_study(n,K,Alpha,6)
stopCluster(cl)
#save(L550b,file="L550b.RData")

set.seed(8574)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
L550c=K_selection_study(n,K,Alpha,6)
stopCluster(cl)
#save(L550c,file="L550c.RData")

set.seed(586393)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
L550d=K_selection_study(n,K,Alpha,6)
stopCluster(cl)
#save(L550d,file="L550d.RData")

set.seed(8636009)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
L550e=K_selection_study(n,K,Alpha,6)
stopCluster(cl)
#save(L550e,file="L550e.RData")

# Combined into a single data set L550
icl_vals=rbind(L550a$icl_vals,L550b$icl_vals,L550c$icl_vals,L550d$icl_vals,L550e$icl_vals)
h_ll_vals=rbind(L550a$h_ll_vals,L550b$h_ll_vals,L550c$h_ll_vals,L550d$h_ll_vals,L550e$h_ll_vals)
runtimes=rbind(L550a$runtimes,L550b$runtimes,L550c$runtimes,L550d$runtimes,L550e$runtimes)
cl_true=rbind(L550a$cl_true,L550b$cl_true,L550c$cl_true,L550d$cl_true,L550e$cl_true)
cl_est=c(L550a$cl_est,L550b$cl_est,L550c$cl_est,L550d$cl_est,L550e$cl_est)
Alpha_est=c(L550a$Alpha_est,L550b$Alpha_est,L550c$Alpha_est,L550d$Alpha_est,L550e$Alpha_est)
L550=list(icl_vals=icl_vals,h_ll_vals=h_ll_vals,runtimes=runtimes,cl_true=cl_true,cl_est=cl_est,Alpha_est=Alpha_est)
#save(L550,file="L550.RData")
table(apply(L550$icl_vals,1,which.max))

# L570 was broken down into 5 parts and combined into a single data set at the end
K=5
n=70
Alpha=matrix(c(1.0,0.7,0.5,0.4,0.6,0.9,1.3,0.7,0.5,0.8,0.6,0.7,1.2,0.8,0.5,0.8,0.6,
               0.4,1.4,0.7,0.7,0.8,0.9,0.6,1.6),nrow=K,ncol=K,byrow=TRUE)

set.seed(77765)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
L570a=K_selection_study(n,K,Alpha,6)
stopCluster(cl)
#save(L570a,file="L570a.RData")

set.seed(666444222)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
L570b=K_selection_study(n,K,Alpha,6)
stopCluster(cl)
#save(L570b,file="L570b.RData")

set.seed(552299)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
L570c=K_selection_study(n,K,Alpha,6)
stopCluster(cl)
#save(L570c,file="L570c.RData")

set.seed(110099)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
L570d=K_selection_study(n,K,Alpha,6)
stopCluster(cl)
#save(L570d,file="L570d.RData")

set.seed(8822882)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
L570e=K_selection_study(n,K,Alpha,6)
stopCluster(cl)
#save(L570e,file="L570e.RData")

# Combined into a single data set L570
icl_vals=rbind(L570a$icl_vals,L570b$icl_vals,L570c$icl_vals,L570d$icl_vals,L570e$icl_vals)
h_ll_vals=rbind(L570a$h_ll_vals,L570b$h_ll_vals,L570c$h_ll_vals,L570d$h_ll_vals,L570e$h_ll_vals)
runtimes=rbind(L570a$runtimes,L570b$runtimes,L570c$runtimes,L570d$runtimes,L570e$runtimes)
cl_true=rbind(L570a$cl_true,L570b$cl_true,L570c$cl_true,L570d$cl_true,L570e$cl_true)
cl_est=c(L570a$cl_est,L570b$cl_est,L570c$cl_est,L570d$cl_est,L570e$cl_est)
Alpha_est=c(L570a$Alpha_est,L570b$Alpha_est,L570c$Alpha_est,L570d$Alpha_est,L570e$Alpha_est)
L570=list(icl_vals=icl_vals,h_ll_vals=h_ll_vals,runtimes=runtimes,cl_true=cl_true,cl_est=cl_est,Alpha_est=Alpha_est)
#save(L570,file="L570.RData")
table(apply(L570$icl_vals,1,which.max))

######################################


