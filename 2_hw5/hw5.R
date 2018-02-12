
data = read.table("/home/dato/workspace/cw_CNLP/2_hw5/data.txt", sep=",", header=FALSE)
y = as.numeric(data)

########################################################################
# 1-1 gaussian mixture fitting k=3

hist(y, probability = TRUE)

init_m <- c(sample(y, 1), sample(y, 1), sample(y, 1))
init_sd <- sqrt( mean((y- mean(y))^2) )
n <- length(y)
k <- 3

# m1, m2, m3, s1, s2, s3, p1, p2, p3
th.old <<- c(init_m[1], init_m[2], init_m[3], init_sd, init_sd, init_sd, 0.3, 0.3, 0.4)

iter <<- 1
h <- matrix(data=NA, nrow=n, ncol=k)

for (i in 1:500) {
  
  # E-step
  p1 <- th.old[7] * dnorm(y, mean=th.old[1], sd=th.old[4])
  p2 <- th.old[8] * dnorm(y, mean=th.old[2], sd=th.old[5])
  p3 <- th.old[9] * dnorm(y, mean=th.old[3], sd=th.old[6])
  
  h[,1] <- p1 / (p1+p2+p3)
  h[,2] <- p2 / (p1+p2+p3)
  h[,3] <- p3 / (p1+p2+p3)
  
  
  # M-step
  h1 <- sum(h[,1])
  h2 <- sum(h[,2])
  h3 <- sum(h[,3])
  
  m1_new <- sum(h[,1]*y) / h1
  m2_new <- sum(h[,2]*y) / h2
  m3_new <- sum(h[,3]*y) / h3
  
  v1_new <- sum(h[,1]*(y-m1_new)^2) / h1
  v2_new <- sum(h[,2]*(y-m2_new)^2) / h2
  v3_new <- sum(h[,3]*(y-m3_new)^2) / h3
  
  pi1_new <- h1 / n
  pi2_new <- h2 / n
  pi3_new <- h3 / n
  
  th.new <- c(m1_new, m2_new, m3_new, 
              sqrt(v1_new), sqrt(v2_new), sqrt(v3_new),
              pi1_new, pi2_new, pi3_new)
  
  
  # Stop condition
  if ( sum( abs(1-th.old/th.new)) < 1e-5 ) {
    th.old <<- th.new
    break
  }
  
  print (th.new)
  th.old <<- th.new
  iter <<- iter + 1
}

# 1-1 result & plot
iter
th.old
hist(y, probability = TRUE)

data_x <- seq(min(y), max(y), length=n)
p1 <- th.old[7] * dnorm(data_x, mean=th.old[1], sd=th.old[4])
p2 <- th.old[8] * dnorm(data_x, mean=th.old[2], sd=th.old[5])
p3 <- th.old[9] * dnorm(data_x, mean=th.old[3], sd=th.old[6])
y_ <- p1+p2+p3

lines(data_x, p1, col="green")
lines(data_x, p2, col="blue")
lines(data_x, p3, col="red")
lines(data_x, y_, col="orange")

sum(log(p1 + p2 + p3))


# 1-2 gaussian mixture fitting k=2

hist(y, probability = TRUE)

init_m <- c(sample(y, 1), sample(y, 1))
init_sd <- sqrt( mean((y- mean(y))^2) )
n <- length(y)
k <- 2

# m1, m2, s1, s2, p1, p2
th.old <<- c(init_m[1], init_m[2], init_sd, init_sd, 0.5, 0.5)

iter <<- 1
h <- matrix(data=NA, nrow=n, ncol=k)

for (i in 1:500) {
  
  # E-step
  p1 <- th.old[5] * dnorm(y, mean=th.old[1], sd=th.old[3])
  p2 <- th.old[6] * dnorm(y, mean=th.old[2], sd=th.old[4])
  
  h[,1] <- p1 / (p1+p2)
  h[,2] <- p2 / (p1+p2)
  
  # M-step
  h1 <- sum(h[,1])
  h2 <- sum(h[,2])
  
  m1_new <- sum(h[,1]*y) / h1
  m2_new <- sum(h[,2]*y) / h2
  
  v1_new <- sum(h[,1]*(y-m1_new)^2) / h1
  v2_new <- sum(h[,2]*(y-m2_new)^2) / h2
  
  pi1_new <- h1 / n
  pi2_new <- h2 / n
  
  th.new <- c(m1_new, m2_new, 
              sqrt(v1_new), sqrt(v2_new),
              pi1_new, pi2_new)
  
  # Stop condition
  if ( sum( abs(1-th.old/th.new)) < 1e-5 ) {
    th.old <<- th.new
    break
  }
  
  print (th.new)
  th.old <<- th.new
  iter <<- iter + 1
}

# 1-2 result & plot
iter
th.old
hist(y, probability = TRUE)

data_x <- seq(min(y), max(y), length=n)
p1 <- th.old[5] * dnorm(data_x, mean=th.old[1], sd=th.old[3])
p2 <- th.old[6] * dnorm(data_x, mean=th.old[2], sd=th.old[4])
y_ <- p1+p2

lines(data_x, p1, col="green")
lines(data_x, p2, col="blue")
lines(data_x, y_, col="orange")

sum(log(p1 + p2))




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
  h <- matrix(data=NA, nrow=n, ncol=k)
  
  
  for (i in 1:500) {
    
    # E-step
    p <- matrix(data=NA, nrow=n, ncol=k)
    for (i in 1:k) {
      p[,i] <- p_old[i] * dnorm(y, mean=m_old[i], sd=s_old[i])
    }
    
    psum <- 0
    for (i in 1:k) {
      psum <- psum + p[,i]
    }
    
    for (i in 1:k) {
      h[,i] <- p[,i] / psum
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
      m_old <- m_new
      s_old <- s_new
      p_old <- p_new
      break
    }
    
    m_old <- m_new
    s_old <- s_new
    p_old <- p_new
    iter <<- iter + 1
  }
}



k <-3
mulGaussian(y, k=2)

# 1-2 result & plot
iter
th.old
hist(y, probability = TRUE)

data_x <- seq(min(y), max(y), length=n)
p1 <- th.old[5] * dnorm(data_x, mean=th.old[1], sd=th.old[3])
p2 <- th.old[6] * dnorm(data_x, mean=th.old[2], sd=th.old[4])
y_ <- p1+p2

lines(data_x, p1, col="green")
lines(data_x, p2, col="blue")
lines(data_x, y_, col="orange")

sum(log(p1 + p2))







