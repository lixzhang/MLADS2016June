# --------------------------------
# 1. functions - glm vs rxGlm
# --------------------------------
# check the data
head(mtcars)
# fit a model with glm(), this can be run on R, MRO, or MRS
# predict V engine vs straight engine with weight and displacement
logistic1 <- glm(vs ~ wt + disp, data = mtcars, family = binomial)
summary(logistic1)

rxOptions(reportProgress = 2) # report progress
# fit the same model with rxGlm(), this can be run on MRS only
# predict V engine vs straight engine with weight and displacement
logistic2 <- rxGlm(vs ~ wt + disp, data = mtcars, family = binomial)
summary(logistic2)

# --------------------------------
# 2. functions - Simulate cluster data
# --------------------------------
if (!require("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library("MASS")
library("ggplot2")

# make sure the results can be replicated
set.seed(112)

# function to simulate data 
simulCluster <- function(nsamples, mean, dimension, group)
{
  Sigma <- diag(1, dimension, dimension)
  x <- mvrnorm(n = nsamples, rep(mean, dimension), Sigma)
  z <- as.data.frame(x)
  z$group = group
  z
}

# simulate data with 2 clusters
nsamples <- 1000
group_a <- simulCluster(nsamples, -1, 2, "a")
group_b <- simulCluster(nsamples, 1, 2, "b")
group_all <- rbind(group_a, group_b)

nclusters <- 2

# plot data 
ggplot(group_all, aes(x = V1, y = V2)) +
  geom_point(aes(colour = group)) +
  geom_point(data = data.frame(V1 = c(-1, 1), V2 = c(-1, 1)), size = 5) +
  xlim(-5, 5) + ylim(-5, 5) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  ggtitle("Simulated data in two overlapping groups")

# assign data 
mydata = group_all[,1:2]

# --------------------------------
# 3. functions - Cluster analysis
# --------------------------------
# cluster analysis with kmeans(), it works on R, MRO, or MRS
system.time(fit_kmeans <- kmeans(mydata, nclusters, iter.max = 1000, algorithm = "Lloyd"))

# cluster analysis with rxKmeans(), it works on MRS only
system.time(fit_rxKmeans <- rxKmeans( ~ V1 + V2, data = mydata,
                          numClusters = nclusters, algorithm = "lloyd"))

# --------------------------------
# 4. functions - Compare results
# --------------------------------                     
# save a dataset in XDF format
dataXDF = tempfile(fileext = ".xdf")
rxImport(inData = mydata, outFile = dataXDF, overwrite = TRUE)
# rxKmeans
fit_rxKmeans_2 <- rxKmeans(~ V1 + V2, data = dataXDF, numClusters = nclusters, algorithm = "lloyd",
                          outFile = dataXDF, outColName = "cluster", overwrite = TRUE)

# convert to dataframe and keep one variable only
cluster_rxKmeans <- rxDataStep(dataXDF, varsToKeep = "cluster")

# append cluster assignment from kmeans and rxKmeans
mydata_clusters <- cbind(group_all,
  cluster_kmeans = factor(fit_kmeans$cluster),
  cluster_rxKmeans = factor(cluster_rxKmeans$cluster))

# compare the cluster assignments between kmeans and rxKmeans
with(mydata_clusters, table(cluster_kmeans, cluster_rxKmeans))

# get cluster centroids 
cluster_centroid_kmeans <- fit_kmeans$centers
cluster_centroid_rxKmeans <- fit_rxKmeans_2$centers

# plot clusters from kmeans
ggplot(mydata_clusters, aes(x = V1, y = V2)) +
  geom_point(aes(colour = cluster_kmeans)) +
  geom_point(data = as.data.frame(cluster_centroid_kmeans), size = 5) +
  xlim(-5, 5) + ylim(-5, 5) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  ggtitle("Clusters found by kmeans()") + 
  theme(legend.title=element_blank())

# plot clusters from rxKmeans
windows()
ggplot(mydata_clusters, aes(x = V1, y = V2)) +
  geom_point(aes(colour = cluster_rxKmeans)) +
  geom_point(data = as.data.frame(cluster_centroid_rxKmeans), size = 5) +
  xlim(-5, 5) + ylim(-5, 5) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  ggtitle("Clusters found by rxKmeans()") +
  theme(legend.title=element_blank())
      
# --------------------------------
# 5. capacity - Simulate data
# --------------------------------          
# make sure the results can be replicated
set.seed(0)

# simulate data and append
nsamples <- 3*10^7 # use this to generate a large dataset
# nsamples <- 10^3 # for testing purpose
group_a <- simulCluster(nsamples, -1, 2, "a")
group_b <- simulCluster(nsamples, 1, 2, "b")
group_all <- rbind(group_a, group_b)

nclusters <- 2

# save data
mydata = group_all[, 1:2]
dataCSV = tempfile(fileext = ".csv")
dataXDF = tempfile(fileext = ".xdf")
write.csv(group_all, dataCSV, row.names = FALSE)
rxImport(inData = dataCSV, outFile = dataXDF, overwrite = TRUE)
                          
# --------------------------------
# 6. capacity - kmeans vs rxKmeans
# --------------------------------          
# cluster analysis with kmeans
system_time_R <- 
  system.time(
    {
      fit <- kmeans(mydata, nclusters,
                    iter.max = 1000,
                    algorithm = "Lloyd")
    })

# cluster analysis with rxKmeans
system_time_MRS <- 
  system.time(
    {
      clust <- rxKmeans( ~ V1 + V2, data = dataXDF,
                         numClusters = nclusters,
                         algorithm = "lloyd",
                         outFile = dataXDF,
                         outColName = "cluster",
                         overwrite = TRUE)
    })

# --------------------------------
# 7. speed - Base R functions (Matrix)
# --------------------------------   
# Initialization
set.seed (1)
m <- 10000
n <-  5000
A <- matrix (runif (m*n),m,n)

# Matrix multiply
system.time (B <- crossprod(A))

# Cholesky Factorization
system.time (C <- chol(B))

# Singular Value Decomposition
m <- 10000
n <- 2000
A <- matrix (runif (m*n),m,n)
system.time (S <- svd (A,nu=0,nv=0))

# Principal Components Analysis
m <- 10000
n <- 2000
A <- matrix (runif (m*n),m,n)
system.time (P <- prcomp(A))

# Linear Discriminant Analysis
library('MASS')
g <- 5
k <- round (m/2)
A <- data.frame (A, fac=sample (LETTERS[1:g],m,replace=TRUE))
train <- sample(1:m, k)
system.time (L <- lda(fac ~., data=A, prior=rep(1,g)/g, subset=train))

# --------------------------------  
# 8. speed - Base R functions (kmeans)
# --------------------------------  
# simulate data 
nsamples <- 10 ^ 7 # this was used on different platforms
# nsamples <- 1000 # for testing purpose
group_a <- simulCluster(nsamples, -1, 2, "a")
group_b <- simulCluster(nsamples, 1, 2, "b")
group_all <- rbind(group_a, group_b)

nclusters <- 2

mydata = group_all[, 1:2]
# K-Means Cluster Analysis
system_time_r <- system.time(fit <- kmeans(mydata, nclusters,
                                           iter.max = 1000,
                                           algorithm = "Lloyd"))

# --------------------------------  
# 9. speed - kmeans vs. rxKmeans                                           
# --------------------------------  
# to save timing results
myresult <- data.frame(nsamples = integer(), time_r = double(), time_rre = double())

# list of sample sizes
nsamples_list <- c(5 * 10 ^ 2, 10 ^ 3, 5 * 10 ^ 3, 10 ^ 4, 5 * 10 ^ 4, 10 ^ 5, 5 * 10 ^ 5, 10 ^ 6, 5 * 10 ^ 6, 10 ^ 7)

for (nsamples in nsamples_list)
{
  # simulate data and append
  group_a <- simulCluster(nsamples, -1, 2, "a")
  group_b <- simulCluster(nsamples, 1, 2, "b")
  group_all <- rbind(group_a, group_b)
  mydata = group_all[, 1:2]

  nclusters <- 2

  # kmeans with R
  system_time_r <- system.time(fit <- kmeans(mydata, nclusters,
                                               iter.max = 1000,
                                               algorithm = "Lloyd"))

  # kmeans with MRS
  system_time_rre <- system.time(clust <- rxKmeans( ~ V1 + V2, data = mydata,
                                                     numClusters = nclusters,
                                                     algorithm = "lloyd"))

  # combine
  newrow <- data.frame(nsamples = nsamples,
                         time_r = as.numeric(system_time_r[3]),
                         time_rre = as.numeric(system_time_rre[3]))
  myresult <- rbind(myresult, newrow)

}
myresult$nsamples <- 2 * myresult$nsamples
mydata <- myresult
mydata$nsamples_log <- log10(mydata$nsamples)
mydata

# generate plot
ggplot(data = mydata, aes(x = nsamples_log)) +
    geom_point(aes(y = time_r, colour = "kmeans")) +
    geom_line(aes(y = time_r, colour = "kmeans")) +
    geom_point(aes(y = time_rre, colour = "rxKmeans")) +
    geom_line(aes(y = time_rre, colour = "rxKmeans")) +
    scale_x_continuous(breaks = seq(2, 8, by = 1)) +
    scale_colour_manual("Function", values = c(kmeans = "red", rxKmeans = "blue")) +
    xlab("log10(number of samples)") +
    ylab("time in seconds") +
    ggtitle("If data fits in memory, \nkmeans() and rxKmeans() are equally performant")
