# Dirichlet SBM on Erasmus programme network data

# Required packages
library(doParallel)
library(igraph)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(LaplacesDemon)
library(circlize)
library(maps)

# Data file "erasmus_network.RData" contains the raw student counts for pairs of countries
# Data file "X_erasmus.RData" is a compositional version of the data, with zero counts in
# "erasmus_network.RData" being replaced by 0.001

load("~/dirSBM/Examples/Erasmus_data/X_erasmus.RData")

n=dim(X_erasmus)[1] # Number of countries
p=n-1 # Dimensionality of Dirichlet observations

set.seed(963682)
cl=makeCluster(detectCores()-2)
registerDoParallel(cl)
cl_erasmus=K_selection(X_erasmus,6)
stopCluster(cl)
save(cl_erasmus,file="cl_erasmus.RData") 
cl_erasmus$ICL

# Results can be found in the file "cl_erasmus.RData" - dirSBM/Examples/Erasmus_data

plot(cl_erasmus$ICL,type="l")
which.max(cl_erasmus$ICL) # K=3 is optimal, followed by K=5



################################################################### K=3 ################################################################### 

dimnames(X_erasmus)[[1]][which(cl_erasmus$cls[3,]==1)] 
dimnames(X_erasmus)[[1]][which(cl_erasmus$cls[3,]==2)] 
dimnames(X_erasmus)[[1]][which(cl_erasmus$cls[3,]==3)] 

# Estimate of Dirichlet parameter matrix with 3 clusters
round(cl_erasmus$Alphas[[3]],digits=3)

# W and V
W3=round(expected_shares(cl_erasmus$Alphas[[3]],cl_erasmus$cls[3,])$W,1)
V3=round(expected_shares(cl_erasmus$Alphas[[3]],cl_erasmus$cls[3,])$V,1)

W3
V3



### Plot K=3 solution on a map
world=map_data("world")

europe=subset(world,region %in% c("Austria", "Belgium", "Bulgaria", "Switzerland", "Cyprus", "Czech Republic", "Germany","Denmark","Estonia", "Spain",
                                  "Finland", "France", "Greece", "Croatia","Hungary", "Ireland", "Iceland", "Italy", "Liechtenstein", "Lithuania",
                                  "Luxembourg", "Latvia", "Malta", "Netherlands","Norway","Poland","Portugal","Romania", "Sweden", "Slovenia","Slovakia",
                                  "Turkey", "UK"))

europe <- subset(europe, lat < 72)

region.lab.data=europe %>%
  group_by(region) %>%
  summarise(long=mean(long),lat=mean(lat))

#pdf("Maps_sols_K=3.pdf",width=7*1.9,height=7)
cols=c(brewer.pal(3,"Set1"))

K5_1=mutate(europe, fill = ifelse(region %in% c("Germany","Spain","France","Italy","UK"), cols[2], cols[1]))

K5_2=mutate(K5_1, fill = ifelse(region %in% c("Austria","Belgium","Czech Republic","Denmark","Finland","Hungary","Netherlands","Poland","Portugal",
                                              "Sweden","Turkey"), cols[3], K5_1$fill))

ggplot(K5_2, aes(long, lat, fill = fill, group=group)) +
  geom_polygon(colour="grey28") +
  scale_fill_manual(values=cols ,name="",labels=c("Germany, Spain, France, Italy, UK",
                                                  "Austria, Belgium, Czech Republic, Denmark, Finland, \nHungary, Netherlands, Poland, Portugal, Sweden, Turkey",
                                                  "Bulgaria, Switzerland, Cyprus, Estonia, Greece, Croatia, \nIreland, Iceland, Liechtenstein, Lithuania, Luxembourg, \nLatvia, Malta, Norway, Romania, Slovenia, Slovakia"))+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5, size = 17),legend.position = "right",legend.key.size = unit(2, "cm"),
        legend.key.width = unit(0.75,"cm"),legend.text = element_text(size=15))
#dev.off()



### Chord diagrams for K=3
dimnames(V3) <- list(paste0("S", 1:3), paste0("R", 1:3))
grid.col = c(S1 = cols[1], S2 = cols[2], S3 = cols[3],
             R1 = cols[1], R2 = cols[2], R3 = cols[3])
circos.clear()
circos.par(start.degree = 90, clock.wise = FALSE)
#pdf("Erasmus_sols_K=3.pdf",width=7.1,height=7.1)
par(cex = 1.5, mar = c(0, 0, 0, 0))
chordDiagram(V3, grid.col = grid.col)
#dev.off()



################################################################### K=5 ################################################################### 

dimnames(X_erasmus)[[1]][which(cl_erasmus$cls[5,]==1)] 
dimnames(X_erasmus)[[1]][which(cl_erasmus$cls[5,]==2)] 
dimnames(X_erasmus)[[1]][which(cl_erasmus$cls[5,]==3)] 
dimnames(X_erasmus)[[1]][which(cl_erasmus$cls[5,]==4)] 
dimnames(X_erasmus)[[1]][which(cl_erasmus$cls[5,]==5)]

# Estimate of Dirichlet parameter matrix with 5 clusters, order swapped to match the K=3 solution
round(cl_erasmus$Alphas[[5]],digits=3)[c(5,2,3,4,1),c(5,2,3,4,1)]

W5=round(expected_shares(cl_erasmus$Alphas[[5]],cl_erasmus$cls[5,])$W,1)[c(5,2,3,4,1),c(5,2,3,4,1)]
V5=round(expected_shares(cl_erasmus$Alphas[[5]],cl_erasmus$cls[5,])$V,1)[c(5,2,3,4,1),c(5,2,3,4,1)]

W5
V5

### Plot K=5 solution on a map
world=map_data("world")

europe=subset(world,region %in% c("Austria", "Belgium", "Bulgaria", "Switzerland", "Cyprus", "Czech Republic", "Germany","Denmark","Estonia", "Spain",
                                  "Finland", "France", "Greece", "Croatia","Hungary", "Ireland", "Iceland", "Italy", "Liechtenstein", "Lithuania",
                                  "Luxembourg", "Latvia", "Malta", "Netherlands","Norway","Poland","Portugal","Romania", "Sweden", "Slovenia","Slovakia",
                                  "Turkey", "UK"))

europe <- subset(europe, lat < 72)

region.lab.data=europe %>%
  group_by(region) %>%
  summarise(long=mean(long),lat=mean(lat))


#pdf("Maps_sols_K=5.pdf",width=7*1.9,height=7)
cols=c(brewer.pal(5,"Set1"))

K5_1=mutate(europe, fill = ifelse(region %in% c("Germany","Spain","France"), cols[2], cols[5]))

K5_2=mutate(K5_1, fill = ifelse(region %in% c("Austria","Belgium","Czech Republic","Denmark","Finland","Italy","Netherlands","Poland","Portugal",
                                              "Sweden","UK"), cols[3], K5_1$fill))

K5_3=mutate(K5_2, fill = ifelse(region %in% c("Bulgaria","Cyprus","Estonia","Greece","Lithuania","Latvia",
                                              "Romania", "Slovenia","Slovakia"), cols[4], K5_2$fill))

K5_4=mutate(K5_3, fill = ifelse(region %in% c("Switzerland","Hungary","Ireland","Norway","Turkey"), cols[1], K5_3$fill))

ggplot(K5_4, aes(long, lat, fill = fill, group=group)) +
  geom_polygon(colour="grey28") +
  scale_fill_manual(values=cols ,name="",labels=c("Germany, Spain, France",
                                                  "Austria, Belgium, Czech Republic, Denmark, Finland, \nItaly, Netherlands, Poland, Portugal, Sweden, UK",
                                                  "Bulgaria, Cyprus, Estonia, Greece, Lithuania, Latvia, \nRomania, Slovenia, Slovakia",
                                                  "Switzerland, Hungary, Ireland, Norway, Turkey",
                                                  "Croatia, Iceland, Liechtenstein, Luxembourg, Malta"))+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5, size = 17),legend.position = "right",legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(0.75,"cm"),legend.text = element_text(size=15))
#dev.off()


### Chord diagrams for K=5
cols=c(brewer.pal(5,"Set1"))
dimnames(V5) <- list(paste0("S", 1:5), paste0("R", 1:5))
grid.col = c(S1 = cols[1], S2 = cols[2], S3 = cols[3], S4 = cols[4], S5 = cols[5],
             R1 = cols[1], R2 = cols[2], R3 = cols[3], R4 = cols[4], R5 = cols[5])
circos.clear()
circos.par(start.degree = 90, clock.wise = FALSE)
#pdf("Erasmus_sols_K=5.pdf",width=7.1,height=7.1)
par(cex = 1.5, mar = c(0, 0, 0, 0))
chordDiagram(V5, grid.col = grid.col)
#dev.off()
