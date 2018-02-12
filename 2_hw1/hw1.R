# generate random sample 

obs = 12
trial = 10
th.true.1 = 0.2
th.true.2 = 0.5

# from uniform random -> generate 1 and 0
z = floor(runif(12, min = 0, max = 1) / 0.5)

y1 = rbinom(n=obs, size=trial, prob=th.true.1)
y2 = rbinom(n=obs, size=trial, prob=th.true.2)

y = (z * y1) + ( ((z+1) %% 2) * y2)

# compute MLE from known label Z
sum(z * y)/sum(10 * z)
sum((1 - z) * y)/sum(10 * (1 - z))


# suppose we don't know label Z
# run EM algorithm

th1.old <- sample(y, 1) / trial
th2.old <- sample(y, 1) / trial

th1.old
th2.old

ll <- list()

for(i in 1:500L) {
  
  ## E-step
  p1 <- 0.5 * dbinom(y, size=10, prob=th1.old)
  p2 <- 0.5 * dbinom(y, size=10, prob=th2.old)
  h <- p1 / (p1+ p2)
  
  ## M-step
  th1.new <- sum(h*y) / sum(10*h)
  th2.new <- sum((1-h)*y) / sum(10*(1-h))
  
  ll[i] = sum(log(p1 + p2))
  
  ## Stop condition
  if( abs(th1.new-th1.old) + abs(th2.new-th2.old) < 1e-7) break
  th1.old <- th1.new
  th2.old <- th2.new
}

list(par=c(th1.new, th2.new), h=h, iter=i)
plot(1:i, ll[1:i], xlab="iteration", ylab="Observed Log-likelihood")

b1 = dbinom(0:10, size=10, prob=th1.new)
b2 = dbinom(0:10, size=10, prob=th2.new)
h = b1 / (b1+b2)
plot(h, xlab="y", ylab="Responsibility")

pdf1 = 0.5 * dbinom(0:10, size=10, prob=th1.new)
pdf2 = 0.5 * dbinom(0:10, size=10, prob=th2.new)

plot(0:10, 0.5*pdf1 + 0.5*pdf2, type="h", xlab="y", ylab="Density")
lines(0:10, 0.5*pdf1, type="o", lty=22, col="red")
lines(0:10, 0.5*pdf2, type="o", lty=22, col="blue")
