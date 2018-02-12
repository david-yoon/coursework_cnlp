
data <- read.delim("/home/datol/cwCNLP/2_hw3/data.txt", sep=",", header=FALSE)
#data <- read.delim("D:\\Dropbox\\sync\\coursework\\CNLP2\\hw3\\data.txt", sep=",", header=FALSE)
y <- as.numeric(data)

hist(y, ylim=c(0, 0.15), breaks=30, probability=T)

n <- length(y)

# 1.normal
u <- sum(y)/n
sigma <- sqrt(sum((y-u)^2)) / n

y_norm = dnorm(1:n, u, sigma)
lines(y_norm, col="purple")


# 2.poisson
lambda <- sum(y)/n

y_poi <- dpois(1:n, lambda = lambda)
lines(y_poi, col="green")



# 3.negative binomial
if(!exists("s_nb", mode="function")) source("Negative_Binomial_Distribution.R")
if(!exists("h_nb", mode="function")) source("Negative_Binomial_Distribution.R")
if(!exists("NRroot_nb", mode="function")) source("Negative_Binomial_Distribution.R")
if(!exists("ll_nb", mode="function")) source("Negative_Binomial_Distribution.R")

ymean = sum(y)/n

r_est = NRroot_nb(0.5, s_nb, h_nb)
p_est = r_est / (r_est + ymean)

y_nb <- dnbinom(1:n, size=r_est, prob=p_est)
lines(y_nb, col="red")

ll_nb(r_est, p_est)



# 4.gamma
if(!exists("s_g", mode="function")) source("Gamma_Distribution.R")
if(!exists("h_g", mode="function")) source("Gamma_Distribution.R")
if(!exists("dg", mode="function")) source("Gamma_Distribution.R")
if(!exists("d2g", mode="function")) source("Gamma_Distribution.R")
if(!exists("NRroot_g", mode="function")) source("Gamma_Distribution.R")
if(!exists("ll_g", mode="function")) source("Gamma_Distribution.R")


th0 <- c(mean(y)^2/var(y), mean(y)/var(y))
result = NRroot_g(th0, s_g, h_g)
alpha = result[1]
beta = result[2]

y_g <-  dgamma(1:121, alpha, beta)
lines(y_g, col="blue")

ll_g(alpha, beta, n)

legend( 80, 0.15,
        c("Normal","Poisson", "Nbinom", "Gamma"),
        lty=c(1,1,1,1),
        col=c("purple", "green", "red", "blue")
)

