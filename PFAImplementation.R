#Implementation of PFA algorithm 
#@param X is input matrix
#@param thresh is threshold of desired variablility to be retained after PCA
#@param add_dim is the desired number of extra columns (as described in paper)
performPrincipalFeatureAnalysis <- function(X, thresh, add_dim = 1){
  #Function cumputing squared Euclidean distance between vectors x and y
  #@param x is an input numeric vector
  #@param y is an input numeric vector 
  computeEuclideanDist <- function(x, y){
    stopifnot(length(x) == length(y))
    return(sum((x - y)^2)) 
  }
  
  #Function computing for a given cluster observation which is closed to the center
  #@param k is the label of cluser
  #@param A is matrix of input data
  #@param labels is a vector of clusters assigned to each observation
  findNearestVector <- function(k, A, labels){
    rownum <- which(labels == k)
    CLUSTER <- A[rownum, , drop = F]
    distances <- apply(CLUSTER, 
                       1, 
                       computeEuclideanDist,
                       centers[k, ])
    minimal_distance <- which.min(distances)
    return(rownum[minimal_distance])
  }
  
  #Apply PCA on correlation matrix
  tmp_pca <- prcomp(x = X,
                    center = T,
                    scale. = T)
  A_q <- tmp_pca$rotation
  
  #Find sufficient number of factors
  sds <- tmp_pca$sd
  cutoff <- min(which(ifelse(cumsum(sds^2 / sum(sds^2)) > thresh, T, F) == T))
  
  #Cut desired number of columns form the rotation matrix and perform kmeans
  A_q <- A_q[, c(1:cutoff)]
  tmpkm <- kmeans(A_q, 
                  centers = ncol(A_q) + add_dim, 
                  iter.max = 100)
  labels <- tmpkm$cluster
  centers <- tmpkm$centers
  
  #Extract features
  features <- sapply(sort(unique(labels)), 
                     findNearestVector, 
                     A = A_q, 
                     labels = labels)
  
  #Return the result
  list(X[, features], features)
}

################################################################################
web_url <- 'https://www.mimuw.edu.pl/~noble/courses/SDA/data/pendigits.txt'
DATA <- read.table(web_url)
DATA <- DATA[, 1:18]

result <- performPrincipalFeatureAnalysis(DATA, 0.8, add_dim = 1)
dim(result[[1]])
result[[2]]
