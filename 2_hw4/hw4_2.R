# Missing Data: Binomial Mixture

set.seed(20161027)
p1 <- 0.2
p2 <- 0.6
pi <- 0.5
n <- 100
ztrue <- rbinom(n, size = 1, prob = pi)
y <- ztrue * rbinom(n, size = 10, prob = p1) + (1 - ztrue) * rbinom(n, size = 10, prob = p2)
z <- ztrue
z[21:length(z)] <- NA
mis <- is.na(z)

# c(p1, p2, pi)
th.old <- c(0.1, 0.7, 0.4)
th.new <- th.old

iter <<- 0

for(i in 1:50L) {

  # E-step
  
  # fill the missing
  z[mis] <- rbinom(80, size = 1, prob = th.old[3])
  
  p1 <- th.old[3] * dbinom(y, size=10, prob=th.old[1])
  p2 <- (1-th.old[3]) * dbinom(y, size=10, prob=th.old[2])
  h <- p1 / (p1+p2)
  
  # M-step
  th.new <- c( sum(h*y) / sum(10*h),
               sum((1-h)*y) / sum(10*(1-h)),
               mean(h) )
  
  iter <<- iter + 1
  
  # stop condition
  if( sum(abs(1-th.old/th.new)) < 1e-5 ) break
  print(th.new)
  th.old <- th.new
  
}