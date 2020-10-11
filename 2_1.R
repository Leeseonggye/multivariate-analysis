# import library
install.packages("gridExtra")
install.packages("ggplot2")
install.packages("clValid")
install.packages("plotrix")
install.packages("lsa")
library(clValid)
library(plotrix)
library(MASS)
library(ggplot2)
library(lsa)
library(gridExtra)

#1-(A)

# Data Generation
Group1_Cov <- matrix(c(0.2,0,0,0.2),2,2)
Group2_Cov <- matrix(c(0.1,0,0,0.1),2,2)
Group3_Cov <- matrix(c(0.1,0,0,0.1),2,2)
Group4_Cov <- matrix(c(0.02,0,0,0.02),2,2)
Group5_Cov <- matrix(c(0.2,0.1,0,0.2),2,2)
Group6_Cov <- matrix(c(0.3,0.1,0,0.3),2,2)

C1<-mvrnorm(n = 1000, c(2, 2), Group1_Cov)
C2<-mvrnorm(n = 1000, c(5, 5), Group2_Cov)
C3<-mvrnorm(n = 1000, c(2, 5), Group3_Cov)
C4<-mvrnorm(n = 1000, c(5, 2), Group4_Cov)
C5<-mvrnorm(n = 1000, c(3.5, 5), Group5_Cov)
C6<-mvrnorm(n = 1000, c(3.5, 3.5), Group6_Cov)
Target<-c(rep(c(1,2,3,4,5,6),each=1000))

Data<-cbind(data.frame(rbind(C1, C2, C3, C4, C5, C6)),Target=Target)

Data_x <- Data[,-3]

Data_x_scaled <- scale(Data_x, center = TRUE, scale = TRUE)

# Evaluating the cluster validity measures
Data_clValid <- clValid(Data_x_scaled, 2:10, clMethods = "kmeans", maxitem=6000, validation = c("internal"))
summary(Data_clValid)

#Dunn index-k=10/silhoutte-k=6


#1-(B)
# Perform K-Means Clustering with the best K determined by Silhouette
Data_kmc <- kmeans(Data_x_scaled,6)
str(Data_kmc)
Data_kmc$centers
Data_kmc$size
Data_kmc$cluster

# Compare the cluster info. and class labels
real_class <- Data$Target
kmc_cluster <- Data_kmc$cluster
table(real_class, kmc_cluster) # K=6

# Perform K-Means Clustering with the best K determined by dunn index
Data_kmc <- kmeans(Data_x_scaled,10)
str(Data_kmc) 
Data_kmc$centers 
Data_kmc$size  
Data_kmc$cluster

# Compare the cluster info. and class labels
real_class <- Data$Target
kmc_cluster <- Data_kmc$cluster
table(real_class, kmc_cluster)

cluster_kmc <- data.frame(Data_x_scaled, clusterID=as.factor(Data_kmc$cluster))
kmc_summary<-data.frame()
