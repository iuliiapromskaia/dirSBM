# Example of use

# Required packages
library(igraph)
library(doParallel)
library(mclust)
library(e1071)
library(LaplacesDemon)

n=50
K=3
Alpha=matrix(c(1,0.6,0.2,0.6,1.5,0.5,0.3,0.4,1.2),nrow=K,ncol=K,byrow=TRUE)

######## Generate synthetic data
data=dirSBM_data_gen(n,K,Alpha)
X=data$X
clusters_true=data$clusters_true
image(X)




######### Fit Dirichlet SBM with 3 clusters
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
mod=dirSBM(X,3)
stopCluster(cl)

# Check estimated clustering
table(clusters_true,mod$cl)
adjustedRandIndex(clusters_true,mod$cl)
image(X[order(mod$cl),rev(order(mod$cl))]) # Data ordered using clustering

# Swap cluster labels to match the order of the true clusters if needed
label_swap=matchClasses(table(clusters_true,mod$cl),method="exact",verbose=0)
label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]

# Check parameter estimates
Alpha # True parameter matrix
mod$Alpha[label_swap,label_swap] # Estimated parameter matrix




######### Run Dirichlet SBM with k=1,...,4 clusters and select the optimal number of clusters
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
cl_data=K_selection(X,4)
stopCluster(cl)

plot(cl_data$ICL,type="l")
K_opt=which.max(cl_data$ICL) # Optimal number of clusters
image(X) # Data in random order
image(X[order(cl_data$cls[K_opt,]),rev(order(cl_data$cls[K_opt,]))]) # Data ordered using clustering

# Check estimated clustering
table(clusters_true,cl_data$cls[K_opt,])
adjustedRandIndex(clusters_true,cl_data$cls[K_opt,])

# Swap cluster labels to match the order of the true clusters if needed
label_swap=matchClasses(table(clusters_true,cl_data$cls[K_opt,]),method="exact",verbose=0)
label_swap=matrix(label_swap,2,K,byrow=TRUE)[1,]

# Check parameter estimates
Alpha # True parameter matrix
cl_data$Alphas[[K_opt]][label_swap,label_swap] # Estimated parameter matrix
