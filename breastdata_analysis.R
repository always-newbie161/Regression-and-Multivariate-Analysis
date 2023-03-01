library("corrr")
library("ggcorrplot")
library("FactoMineR")
library("factoextra")
library("psych")

breast_data <- read.csv("breast_data.csv")

breast_data_cont = breast_data[3:length(breast_data)-1]

breast_data_cont_scaled = scale(breast_data_cont)

breast_corr <- cor(breast_data_cont_scaled)

ggcorrplot(corr_matrix)


## PC ANALYSIS
breast_data.pca <- princomp(breast_corr)
summary(breast_data.pca)

#plots
fviz_eig(breast_data.pca, addlabels = TRUE)
fviz_pca_var(breast_data.pca, col.var = "cos2",gradient.cols = c("black", "orange", "green"))


##### FACTOR ANALYSIS
fa.parallel(breast_corr, n.obs=112, fa="both", n.iter=100,
            main="Scree plots with parallel analysis")
            
fa <- fa(breast_corr, nfactors=2, rotate="none", fm="pa")
fa



fa <- fa(breast_corr, nfactors=2, rotate="varimax", fm="pa")
fa


##### CLUSTER ANALYSIS

# Plotting clusters for different k
k2 <- kmeans(breast_data_cont_scaled, centers = 2, nstart = 25)
k3 <- kmeans(breast_data_cont_scaled, centers = 3, nstart = 25)
k4 <- kmeans(breast_data_cont_scaled, centers = 4, nstart = 25)
k5 <- kmeans(breast_data_cont_scaled, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = breast_data_cont_scaled) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = breast_data_cont_scaled) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = breast_data_cont_scaled) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = breast_data_cont_scaled) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)


# Finding optimal clusters using elbow method
set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(breast_data_cont_scaled, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 20
k.values <- 1:20

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
       type="b", pch = 19, frame = FALSE, 
       xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares")
       
       
#siloutte method for optimal clustering

# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(breast_data_cont_scaled, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(breast_data_cont))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
       type = "b", pch = 19, frame = FALSE, 
       xlab = "Number of clusters K",
       ylab = "Average Silhouettes")
       


# Optimal is 2 clusters so going with k=2.
k2$cluster
k2$centers
k2$withinss
fviz_cluster(k2, data = breast_data_cont_scaled)





