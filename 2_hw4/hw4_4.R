# # hw4-4 3-D gaussian mixture

# generate data
set.seed(2016)
m <- c(1, 4, 9)
s <- c(1, 2, 1)
pi <- c(0.3, 0.3, 0.4)
k <- 3
n <- 1000


z <- sample(1:k, size=n, prob=pi, replace=TRUE)
y <- numeric(n)
for(i in 1:n) y[i] <- rnorm(1, m[z[i]], s[z[i]])

hist(y, probability = TRUE)

init_m <- c(sample(y, 1), sample(y, 1), sample(y, 1))
init_sd <- sqrt( mean((y- mean(y))^2) )


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