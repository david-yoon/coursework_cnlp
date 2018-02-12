# exercise 6.8

rm(list=ls())

# K-means
# X: N x p matrix, K: number of cluster
km <- function(X, K, maxiter = 50L, tol = 1e-8) {
  
  N <- nrow(X)
  p <- ncol(X)
  mu0 <- X[sample.int(N, K), ]
  mu1 <- mu0
  
  d <- matrix(0, N, K)
  h <- matrix(0, N, K)
  
  for(i in 1:maxiter) {
    ## assign data points to clusters (E Step)
    for(n in 1:N) {
      for(k in 1:K) {
        d[n, k] <- sum((X[n,] - mu0[k,])^2)
      }
      h[n, ] <- min(d[n,]) == d[n,]
    }
    ## compute the cluster means (M Step)
    for(j in 1:p) {
      for(k in 1:K) {
        mu1[k, j] <- sum(h[,k] * X[,j]) / sum(h[,k])
      }
    }
    ## stop condition
    if(sum(abs(1 - mu0/mu1)) < tol) break
    mu0 <- mu1
  }
  list(centers = mu1, clusters = h, iter = i)
}

## Gaussian
require(mvtnorm)
gaussianmix <- function(X, K, maxiter=500L, tol=1e-8) {
  N <- nrow(X)
  d <- ncol(X)
  m0 <- X[sample.int(N, K), ]
  S0 <- array(cov(X), c(d, d, K))
  p0 <- rep(1/K, K)
  h <- matrix(NA, N, K)
  m1 <- matrix(NA, K, d)
  S1 <- array(NA, c(d, d, K))
  p1 <- rep(NA, K)
  for(i in 1:maxiter) {
    for(k in 1:K) {
      h[,k] <- p0[k] * mvtnorm::dmvnorm(X, mean=as.matrix(m0[k,]), sigma=as.matrix(S0[,,k]))
    }
    h <- prop.table(h, margin=1)
    
    for(k in 1:K) {
      Nk <- sum(h[,k])

      for(j in 1:d) {
        m1[k, j] <- sum(h[,k] * X[,j]) / Nk
      }
      
      Xc <- X - matrix(m1[k,], N, d, byrow=T)
      S1[,,k] <- t(matrix(h[,k], N, d) * Xc) %*% as.matrix(Xc) / Nk
      
      p1[k] <- Nk / N
    }
    
    if(sum(abs(1-S0/S1)) + sum(abs(1-m0/m1)) + sum(abs(1-p0/p1)) < tol) break
    S0 <- S1
    m0 <- m1
    p0 <- p1
  }
  list(centers=m1, covariances=S1, prob=p1, iter=i, h=h)
  
}


#data <- read.table("/home/dato/workspace/cw_CNLP/2_hw6/faithful.txt", header=TRUE)
data <- read.table("D:\\09_develope\\workspace\\cw_CNLP\\2_hw6\\faithful.txt", header=TRUE)


K <- 2


# K-means
ret = km(data, K)
plot(data, col=ifelse(ret$clusters[,1], "red", "blue"))
for(i in 1:K) {
  points(x=ret$centers[i,][1], y=ret$centers[i,][2], pch=15, col="black")
}



# Gaussian
ret_gau = gaussianmix(data, K)
ret_gau$centerspoints(ret_gau$centers[1,], pch=15, col="black")
ret_gau$covariances
clusters = round(ret_gau$h, digits=0)

plot(data, col=ifelse(clusters[,1], "red", "blue"))
lines(ellipse::ellipse(centre=ret_gau$centers[1,], x=ret_gau$covariances[,,1], type="l"))
lines(ellipse::ellipse(centre=ret_gau$centers[2,], x=ret_gau$covariances[,,2], type="l"))

for(i in 1:K) {
  points(x=ret_gau$centers[i,][1], y=ret_gau$centers[i,][2], pch=15, col="black")
}


