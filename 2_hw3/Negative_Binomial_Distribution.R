
# Negative Binomial Distribution
# r=3, prob=0.1
# y = number of failures before r-th success

set.seed(12345)
n=1000
y <- rnbinom(n, size=3, prob=0.1)
hist(y, breaks=30, probability=T)


#dg <- function(x, h=1e-7) (gamma(x+h) - gamma(x))/h
#d2g <- function(x, h=1e-7) (gamma(x+2*h) - 2*gamma(x+h) + gamma(x))/h^2


ymean = sum(y)/n

s_nb <- function (r) {
  ds <- sum(digamma(y+r)) - n*digamma(r) + n*log((r/(r+ymean)))
  return(ds)
}


h_nb <- function (r) {
  dh <- sum(trigamma(y+r)) - n*trigamma(r) + n*( 1/r - 1/(r+ymean) )
  return(dh)
}


ll_nb <- function(r, p) {
  p = r / (r+ymean)
  sum(log( gamma(y+r) ) -log(gamma(r)) -log(factorial(y)) + r*log(p) + y*log(1-p))
}


NRroot_nb <- function(x0, S, H) {
  x1 <- x0 - S(x0) / H(x0)
  if( sum(abs(1-x0/x1)) < 1e-8 ) {
   x1
  }
  else {
    cat(x1)
    cat(", ")
    NRroot_nb(x1, S, H)
  }
}

r0 <- 0.5

r_est = NRroot_nb(r0, s_nb, h_nb)
p_est = r_est / (r_est + ymean)

param <- list(0.8593201, 1.394143, 2.055956, 2.639004, 2.914576, 2.956065, 2.956839, 2.956839)

# plot trace
res <- list()
for (i in 1:length(param) ) {
  res[i] = ll_nb(as.numeric(param[i]))
}
plot(1:length(res), res)


# plot histogram of the original data
y <- rnbinom(n, size=3, prob=0.1)
hist(y, ylim=c(0, 0.03), breaks=30, probability=T)

# plot pdf of the predicted result
y2 <- dnbinom(1:1000, size=r_est, prob=p_est)
lines(y2)

