
# Gamma Distribution
# shape=3, rate=4

set.seed(12345)
n=1000
y <- rgamma(n, shape=3, rate=4)
hist(y, breaks=30, probability=T)

dg <- function(x, h=1e-7) (gamma(x+h) - gamma(x))/h
d2g <- function(x, h=1e-7) (gamma(x+2*h) - 2*gamma(x+h) + gamma(x))/h^2


s_g <- function (x) {
  dalpha <- sum(log(y)) + n * log(x[2]) - n * dg(x[1]) / gamma(x[1])
  dbeta <- -sum(y) + n * x[1] * x[2]^(-1)
  return( c(dalpha, dbeta) )
}

h_g <- function (x) {
  dalpha2 <- -n * (d2g(x[1]) * gamma(x[1]) - dg(x[1])^2) / gamma(x[1])^2
  dalphabeta <- n * x[2]^(-1)
  dbeta2 <- -n * x[1] * x[2]^(-2)
  
  A = matrix(
    c(dalpha2, dalphabeta, dalphabeta, dbeta2),
    nrow=2, ncol=2, byrow=TRUE
  )
  
  return(A)
}

ll_g <- function(alpha, beta, n) {
  (alpha-1)*sum(log(y)) - beta*sum(y) + n*alpha*log(beta) - n*log( gamma(alpha))
}


NRroot_g <- function(x0, S, H) {
  x1 <- x0 - solve(H(x0), S(x0))
  if( sum(abs(1-x0/x1)) < 1e-8 ) {
    x1
  }
  else {
    cat(x1[2])
    cat(", ")
    NRroot_g(x1, S, H)
  }
}

th0 <- c(mean(y)^2/var(y), mean(y)/var(y))


result = NRroot_g(th0, s_g, h_g)
alpha = result[1]
beta = result[2]

param_alpha <-list(3.027517, 3.026405, 3.025782, 3.024883, 3.024918, 3.024936, 3.024942, 3.024941, 3.024941)
param_beta <-list(4.196775, 4.195233, 4.19437, 4.193124, 4.193172, 4.193197, 4.193205, 4.193205, 4.193205)

res <- list()
# plot trace
for (i in 1:length(param_alpha) ) {
  res[i] = ll_g(as.numeric(param_alpha[i]), as.numeric(param_beta[i]), 1000)
}
plot(1:length(res), res)


y <- rgamma(n, shape=3, rate=4)
hist(y, breaks=30, probability=T)

y2 <- dgamma(1:n, alpha, beta)
lines(y2)
sum(y2)

y3 <- rgamma(n, shape=alpha, rate=beta)
lines(density(y3))

