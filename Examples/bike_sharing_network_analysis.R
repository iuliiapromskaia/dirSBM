# Dirichlet SBM on London bike sharing network data

# Required packages
library(doParallel)
library(igraph)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(LaplacesDemon)
library(circlize)

# Data file "bike_network.RData" contains the raw counts of trips taken between the pairs of bike stations in 2014.
# The network has 85 stations as these are the stations that are both in the top 100 most popular start stations and
# in the top 100 most popular end stations.
# Data file "X_bike.RData" is a compositional version of the data, with zero counts in "bike_network.RData" being
# replaced by 0.001

load("~/dirSBM/Examples/Bike_sharing_data/X_bike.RData")

n=dim(X_bike)[1] # Number of stations
p=n-1 # Dimensionality of Dirichlet observations

set.seed(888222)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
cl_bike=K_selection(X_bike,7)
stopCluster(cl)
#save(cl_bike,file="cl_bike.RData")

# Results can be found in the file "cl_bike.RData" - dirSBM/Examples/Bike_sharing_data

plot(cl_bike$ICL,type="l")
which.max(cl_bike$ICL)
image(X_bike[order(cl_bike$cls[4,]),rev(order(cl_bike$cls[4,]))],col=brewer.pal(n=9,name="OrRd"),xaxt="n",yaxt="n",
      main="K=4")

################## Matrices W and V that help to interpret the model parameters

round(cl_bike$Alphas[[4]],digits=2)

W=round(expected_shares(cl_bike$Alphas[[4]],cl_bike$cls[4,])$W,1)
V=round(expected_shares(cl_bike$Alphas[[4]],cl_bike$cls[4,])$V,1)

V
W


### Chord diagram for K=4
cols=c("#0F9D58","#0288D1", "#E65100","#9C27B0") # Colors to match Google My Maps plot
dimnames(V) <- list(paste0("S", 1:4), paste0("R", 1:4))
grid.col = c(S1 = cols[1], S2 = cols[2], S3 = cols[3], S4 = cols[4],
             R1 = cols[1], R2 = cols[2], R3 = cols[3], R4 = cols[4])
circos.clear()
circos.par(start.degree = 90, clock.wise = FALSE)
#pdf("bike_sols_K=4.pdf",width=7.1,height=7.1)
par(cex = 1.5, mar = c(0, 0, 0, 0))
chordDiagram(V, grid.col = grid.col)
#dev.off()

