# hw4-3 Normal Mixture Model

y <- c(-0.39, 0.12, 0.94, 1.67, 1.76, 2.44, 3.72, 4.28, 4.92, 5.53,0.06, 0.48, 1.01, 1.68, 1.80, 3.25, 4.12, 4.60, 5.28, 6.22)

hist(y, probability=T)


fit.twnorm <- function (y, pars) {
  
  iter <<- 1
  loglik <<- list()
  
  # pi, m1, s1, m2, s2
  var <- mean( (y- mean(y))^2 )
  sd <- sqrt(var)
  n <- length(y)
  
  # initial value
  #th.old <- c(0.5, pars[1], sd, pars[2], sd)
  th.old <<- c(0.5, 1, sd, 6, sd)
  
  for (i in 1:500L) {
    
    # E-step
    p1 <- (1-th.old[1]) * dnorm(y, th.old[2], th.old[3])
    p2 <- th.old[1] * dnorm(y, th.old[4], th.old[5])
    h <<- p2 / (p1 + p2)
    
    # M-step
    u1 <- sum((1-h)*y) / sum(1-h)
    u2 <- sum(h*y) / sum(h)
    
    var1 <- sum( (1-h)*(y-u1)^2 ) / sum(1-h)
    var2 <- sum( h*(y-u2)^2 ) / sum(h)
    
    th.new <- c(mean(h), u1, sqrt(var1), u2, sqrt(var2))
    
    loglik[i] <<- sum(log(p1 + p2))
    
    # Stop condition
    if ( sum( abs(1-th.old/th.new)) < 1e-5 ) {
      th.old <<- th.new
      break
    }

    print (th.new)
    th.old <<- th.new
    iter <<- iter + 1
    
  }
}
  
iter = 0
fit.twnorm(y, c(1, 6))
th.old
loglik[length(loglik)]
plot(1:length(loglik), loglik)
iter

iter = 0
fit.twnorm(y, c(-10, 5))
th.old
loglik[length(loglik)]
plot(1:length(loglik), loglik)
iter

data_x <- seq(min(y), max(y), length=20)
p1 <- (1-th.old[1]) * dnorm(data_x, mean=th.old[2], sd=th.old[3])
p2 <- th.old[1] * dnorm(data_x, mean=th.old[4], sd=th.old[5])
y_ <- p1+p2

hist(y, probability=T, xlim = range(-2, 8))
lines(data_x, p1, col="green")
lines(data_x, p2, col="blue")
lines(data_x, y_, col="red")

# responsibility
h = p2 / (p1+p2)
plot(h)


# empirical cdf
plot(ecdf(y))
