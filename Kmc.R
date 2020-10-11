# Package for cluster validity
install.packages("clValid")
install.packages("plotrix")

library(clValid) #��ǥ���� �����ؼ� ����� �ϴ� ��Ű��, ����ȭ ������� Ÿ�缺����
library(plotrix) # �׸� �׷��ִ� ��Ű��

# Part 1: K-Means Clustering ----------------------------------------------
# Load the Wine dataset
# Remove the class label
outlier <- read.csv("C:/Users/�̼���/Desktop/outlier.csv")
str(outlier)

outlier_x <- outlier[,-1]
outlier_xx <- outlier_x[,-3]

# data scaling
outlier_xx_scaled <- scale(outlier_xx, center = TRUE, scale = TRUE)
# ���������� �������� �ٸ��Ƿ� ����ȭ�� ������Ѵ�. ����ȭ�ۤ�
# Evaluating the cluster validity measures
outlier_clValid <- clValid(outlier_xx_scaled, 2:10, clMethods = "kmeans", 
                        validation = c("internal"),maxitems = 300)
summary(outlier_clValid)
# Perform K-Means Clustering with the best K determined by Silhouette
outlier_kmc <- kmeans(outlier_xx_scaled,5)
str(outlier_kmc)
outlier_kmc$centers
outlier_kmc$size 
outlier_kmc$cluster

real_class <- outlier_class
kmc_cluster<- outlier_kmc$cluster
table(real_class, kmc_cluster)

#plotting
plot(outlier_xx)
points(outlier_xx,col=c(outlier_kmc),cex=2)
plot(outlier_kmc)
plot(outlier_kmc$cluster)

plot(outlier_xx[c("X.points","X.dim.")], col=kmc$cluster)

#�� Ŭ�������� �߽��� �׸���

points(kc$centers[,c("Sepal.Length","Sepal.Width")],col=1:3,pch=8,cex=2)



# Compare the cluster info. and class labels
real_class <- wine_class
kmc_cluster <- wine_kmc$cluster
table(real_class, kmc_cluster)

# Compare each cluster for KMC
cluster_kmc <- data.frame(wine_x_scaled, clusterID = as.factor(wine_kmc$cluster)) 
kmc_summary <- data.frame() #���Ӹ� ���̺� ����, �ʱ�ȭ �ϳ��� ������ ���̰ڤ�

for (i in 1:(ncol(cluster_kmc)-1)){
  kmc_summary = rbind(kmc_summary, 
                      tapply(cluster_kmc[,i], cluster_kmc$clusterID, mean))
}
# tapply�� �������� ���ؼ� ���� i��° �÷��� �����ͼ� ����� ���ض�

colnames(kmc_summary) <- paste("cluster", c(1:3))
rownames(kmc_summary) <- colnames(wine_x)
kmc_summary

# Radar chart
par(mfrow = c(1,3)) #�� ȭ�鿡 �׸��� ������ �׸��� �Լ�
for (i in 1:3){
  plot_title <- paste("Radar Chart for Cluster", i, sep=" ")
  radial.plot(kmc_summary[,i], labels = rownames(kmc_summary), 
              radial.lim=c(-2,2), rp.type = "p", main = plot_title, 
              line.col = "red", lwd = 3, show.grid.labels=1)
} #��������� ���������� �� ū�� Ȯ���ϴ� �׸�
dev.off()

# Compare the first and the second cluster
kmc_cluster1 <- outlier_xx[outlier_kmc$cluster == 1,]
kmc_cluster2 <- outlier_xx[outlier_kmc$cluster == 2,]

# t_test_result
kmc_t_result <- data.frame()

for (i in 1:13){
  
  kmc_t_result[i,1] <- t.test(kmc_cluster1[,i], kmc_cluster2[,i], 
                              alternative = "two.sided")$p.value
  
  kmc_t_result[i,2] <- t.test(kmc_cluster1[,i], kmc_cluster2[,i], 
                              alternative = "greater")$p.value
  
  kmc_t_result[i,3] <- t.test(kmc_cluster1[,i], kmc_cluster2[,i], 
                              alternative = "less")$p.value
}

kmc_t_result