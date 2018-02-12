# example 6.6

rm(list=ls())

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

ret_gau = gaussianmix(data, K)
ret_gau$centers
ret_gau$covariances
ret_gau$prob
clusters = round(ret_gau$h, digits=0)

plot(data, col=ifelse(clusters[,1], "red", "blue"))
lines(ellipse::ellipse(centre=ret_gau$centers[1,], x=ret_gau$covariances[,,1], type="l"))
lines(ellipse::ellipse(centre=ret_gau$centers[2,], x=ret_gau$covariances[,,2], type="l"))

for(i in 1:K) {
  points(x=ret_gau$centers[i,][1], y=ret_gau$centers[i,][2], pch=15, col="black")
}



# exercise 6.9 Common Covariance model

gaussianmix_common_cov <- function(X, K, maxiter=500L, tol=1e-8) {
  
  N <- nrow(X)
  d <- ncol(X)
  m0 <- X[sample.int(N, K), ]
  
  # common covariance model
  S0 <- array(cov(X), c(d, d))
  p0 <- rep(1/K, K)
  h <- matrix(NA, N, K)
  m1 <- matrix(NA, K, d)
  S1 <- array(NA, c(d, d))
  p1 <- rep(NA, K)
  for(i in 1:maxiter) {
    for(k in 1:K) {
      # common covariance model
      h[,k] <- p0[k] * mvtnorm::dmvnorm(X, mean=as.matrix(m0[k,]), sigma=as.matrix(S0[,]))
    }
    h <- prop.table(h, margin=1)
    
    cov <- matrix(c(0, 0, 0, 0), ncol=2)
    
    for(k in 1:K) {
      Nk <- sum(h[,k])
      
      for(j in 1:d) {
        m1[k, j] <- sum(h[,k] * X[,j]) / Nk
      }
    
      hh <- round(h, 0)
      cluster <- X[,] * hh[,k]
      cov <- cov + Nk * cov(cluster[hh[,k]==1,])
      
      p1[k] <- Nk / N
    }
    
    S1 <- cov / N
    
    if(sum(abs(1-S0/S1)) + sum(abs(1-m0/m1)) + sum(abs(1-p0/p1)) < tol) break
    S0 <- S1
    m0 <- m1
    p0 <- p1
  }
  list(centers=m1, covariances=S1, prob=p1, iter=i, h=h)
  
}

data <- read.table("D:\\09_develope\\workspace\\cw_CNLP\\2_hw6\\faithful.txt", header=TRUE)

K <- 2

ret_gau = gaussianmix_common_cov(data, K)
ret_gau$centers
ret_gau$covariances
ret_gau$prob
clusters = round(ret_gau$h, digits=0)

plot(data, col=ifelse(clusters[,1], "red", "blue"))
lines(ellipse::ellipse(centre=ret_gau$centers[1,], x=ret_gau$covariances[,], type="l"))
lines(ellipse::ellipse(centre=ret_gau$centers[2,], x=ret_gau$covariances[,], type="l"))

for(i in 1:K) {
  points(x=ret_gau$centers[i,][1], y=ret_gau$centers[i,][2], pch=15, col="black")
}


# exercise 6.10 Diagonal Covariance model

gaussianmix_diagonal_cov <- function(X, K, maxiter=500L, tol=1e-8) {
  
  N <- nrow(X)
  d <- ncol(X)
  m0 <- X[sample.int(N, K), ]
  
  # common covariance model
  S0 <- array(cov(X), c(d, d))
  S0[2] = 0
  S0[3] = 0
  p0 <- rep(1/K, K)
  h <- matrix(NA, N, K)
  m1 <- matrix(NA, K, d)
  S1 <- array(NA, c(d, d))
  p1 <- rep(NA, K)
  for(i in 1:maxiter) {
    for(k in 1:K) {
      # common covariance model
      h[,k] <- p0[k] * mvtnorm::dmvnorm(X, mean=as.matrix(m0[k,]), sigma=as.matrix(S0[,]))
    }
    h <- prop.table(h, margin=1)
    
    cov <- matrix(c(0, 0, 0, 0), ncol=2)
    
    for(k in 1:K) {
      Nk <- sum(h[,k])
      
      for(j in 1:d) {
        m1[k, j] <- sum(h[,k] * X[,j]) / Nk
      }
      
      hh <- round(h, 0)
      cluster <- X[,] * hh[,k]
      cov <- cov + Nk * diag(cov(cluster[hh[,k]==1,]))
      
      p1[k] <- Nk / N
    }
    
    S1 <- cov / N
    S1[2] = 0
    S1[3] = 0
    
    if(sum(abs(1- diag(S0)/diag(S1) )) + sum(abs(1-m0/m1)) + sum(abs(1-p0/p1)) < tol) break
    S0 <- S1
    m0 <- m1
    p0 <- p1
  }
  list(centers=m1, covariances=S1, prob=p1, iter=i, h=h)
  
}

data <- read.table("D:\\09_develope\\workspace\\cw_CNLP\\2_hw6\\faithful.txt", header=TRUE)

K <- 2

ret_gau = gaussianmix_diagonal_cov(data, K)
ret_gau$center
ret_gau$covariances
ret_gau$prob
clusters = round(ret_gau$h, digits=0)

plot(data, col=ifelse(clusters[,1], "red", "blue"))
lines(ellipse::ellipse(centre=ret_gau$centers[1,], x=ret_gau$covariances[,], type="l"))
lines(ellipse::ellipse(centre=ret_gau$centers[2,], x=ret_gau$covariances[,], type="l"))

for(i in 1:K) {
  points(x=ret_gau$centers[i,][1], y=ret_gau$centers[i,][2], pch=15, col="black")
}