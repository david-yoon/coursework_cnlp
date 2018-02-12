# Missing Data: Bivariate Normal

x1 <- c(NA, NA, 0, 17, 6,  15,  1,  4,  0, 20, 13, 21, 23, 8, 17, 9)
x2 <- c(27, 16, 9, 33, 15, 34, 16, 18, 20, 32, 19, 32, 23, 24, 23, 13)

plot(x1, x2)

n <- length(x1)
mis <- is.na(x1)

# sufficient statistics
x1sq <- x1^2
x2sq <- x2^2

# initialize
# u1, u2, s1^2, s2^2, lo
th.old <- c(mean(x1[!mis]), mean(x2), sd(x1[!mis]), sd(x2), 0.5)

for(i in 1:50L) {
  
  # E-step
  x1[mis] <- th.old[1] + th.old[5]*sqrt( th.old[3]/ th.old[4] )*(x2[mis] - th.old[2] )
  x1sq[mis] <- th.old[3] + x1[mis]^2
  
  # M-step
  s1 <- sum(x1)
  s2 <- sum(x2)
  s11 <- sum(x1sq)
  s22 <- sum(x2sq)
  s12 <- sum(x1[!mis]*x2[!mis]) + sum( x2[mis]*x1[mis] )
  
  th.new <- c(s1/n, 
              s2/n, 
              s11/n - (s1/n)^2, 
              s22/n - (s2/n)^2, 
              (s12/n - s1/n*s2/n) / sqrt(s11/n - (s1/n)^2) / sqrt(s22/n - (s2/n)^2) )
  
  ## Stop condition
  if( sum(abs(1-th.old/th.new)) < 1e-5 ) break
  print(th.new)
  th.old <- th.new
}

x1[mis]

