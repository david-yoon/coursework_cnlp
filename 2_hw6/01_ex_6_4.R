# exercise 6.4

rm(list=ls())
#data = read.table("/home/dato/workspace/cw_CNLP/2_hw5/data.txt", sep=",", header=FALSE)
data = read.table("D:\\09_develope\\workspace\\cw_CNLP\\2_hw5\\data.txt", sep=",", header=FALSE)

y = as.numeric(data)
########################################################################

hist(y, probability = TRUE)

mulGaussian <- function (y, k) {
  
  # parameters for gaussian
  m_old <- vector()
  m_new <- vector()
  
  s_new <- vector()
  s_old <- vector()
  
  p_new <- vector()
  p_old <- vector()
  
  n <- length(y)
  init_sd <- sqrt( mean((y- mean(y))^2) )
  
  for (i in 1:k) {
    m_old[i] <- sample(y, 1)
    s_old[i] <- init_sd
    p_old[i] <- 1/k
  }
  
  iter <<- 1
  h <<- matrix(data=NA, nrow=n, ncol=k)
  p <<- matrix(data=NA, nrow=n, ncol=k)
  
  for (i in 1:500) {
    
    # E-step
    for (i in 1:k) {
      p[,i] <<- p_old[i] * dnorm(y, mean=m_old[i], sd=s_old[i])
    }
    
    psum <- 0
    for (i in 1:k) {
      psum <- psum + p[,i]
    }
    
    for (i in 1:k) {
      h[,i] <<- p[,i] / psum
    }
    
    # M-step
    for (i in 1:k) {
      m_new[i] <- sum(h[,i]*y) / sum(h[,i])
      s_new[i] <- sum(h[,i]*(y-m_new[i])^2) / sum(h[,i])
      s_new[i] <- sqrt(s_new[i])
      p_new[i] <- sum(h[,i]) / n
    }
    
    # Stop condition
    cond_sum <- 0
    for (i in 1:k) {
      cond_sum <- cond_sum + 1 - m_old[i] / m_new[i]
      cond_sum <- cond_sum + 1 - s_old[i] / s_new[i]
      cond_sum <- cond_sum + 1 - p_old[i] / p_new[i]
    }
    
    if ( abs(cond_sum) < 1e-5 ) {
      return (c(m_new, s_new, p_new))
    }
    
    m_old <- m_new
    s_old <- s_new
    p_old <- p_new
    iter <<- iter + 1
  }
}

################################################################
# 1-1 1-d case
k <- 1
pars <- mulGaussian(y, k)
iter
pars
hist(y, probability = TRUE)

data_x <- seq(min(y), max(y), length=length(y))
p1 <- pars[3] * dnorm(data_x, mean=pars[1], sd=pars[2])

lines(data_x, p1, col="green")
lik <- sum (log(p1))
AIC1 <- -2*lik + 2*k
BIC1 <- -2*lik + k*log(length(y))

################################################################
# 1-2 2-d case
k <- 2
pars <- mulGaussian(y, k)
iter
pars
hist(y, probability = TRUE)

data_x <- seq(min(y), max(y), length=length(y))
p1 <- pars[5] * dnorm(data_x, mean=pars[1], sd=pars[3])
p2 <- pars[6] * dnorm(data_x, mean=pars[2], sd=pars[4])
y_ <- p1+p2

lines(data_x, p1, col="green")
lines(data_x, p2, col="blue")
lines(data_x, y_, col="orange")

lik <- sum(log(p1 + p2))
AIC2 <- -2*lik + 2*k
BIC2 <- -2*lik + k*log(length(y))

################################################################
# 1-3 3-d case
k <- 3
pars <- mulGaussian(y, k)
iter
pars
hist(y, probability = TRUE)

data_x <- seq(min(y), max(y), length=length(y))
p1 <- pars[7] * dnorm(data_x, mean=pars[1], sd=pars[4])
p2 <- pars[8] * dnorm(data_x, mean=pars[2], sd=pars[5])
p3 <- pars[9] * dnorm(data_x, mean=pars[3], sd=pars[6])
y_ <- p1 +p2 +p3

lines(data_x, p1, col="green")
lines(data_x, p2, col="blue")
lines(data_x, p3, col="red")
lines(data_x, y_, col="orange")

lik <- sum(log(p1 + p2 + p3))
AIC3 <- -2*lik + 2*k
BIC3 <- -2*lik + k*log(length(y))

################################################################
# 1-4 4-d case
k <- 4
pars <- mulGaussian(y, k)
iter
pars
hist(y, probability = TRUE)

data_x <- seq(min(y), max(y), length=length(y))
p1 <- pars[9] * dnorm(data_x, mean=pars[1], sd=pars[5])
p2 <- pars[10] * dnorm(data_x, mean=pars[2], sd=pars[6])
p3 <- pars[11] * dnorm(data_x, mean=pars[3], sd=pars[7])
p4 <- pars[12] * dnorm(data_x, mean=pars[4], sd=pars[8])
y_ <- p1 +p2 +p3+ p4

lines(data_x, p1, col="green")
lines(data_x, p2, col="blue")
lines(data_x, p3, col="red")
lines(data_x, p4, col="gray")
lines(data_x, y_, col="orange")

lik <- sum(log(p1 + p2 + p3 + p4))
AIC4 <- -2*lik + 2*k
BIC4 <- -2*lik + k*log(length(y))

c(AIC1, AIC2, AIC3, AIC4)
c(BIC1, BIC2, BIC3, BIC4)