library(ggplot2)
library(kableExtra)
library(dplyr)
# Question 01 -------------------------------------------------------------
set.seed(3)
# Find I by calculus techniques.
I <- function(x){
  return(exp(-abs(x)))
}
Ans1 <- integrate(I, -20, 20)
Ans1

# Find I by simultaion 

N <- 5000 # 5000 samples of size 1000.
n <- 1000

# (1) Monte Carlo

theta.hat <- NULL
for( i in 1:N){
x <- runif(n, -20, 20)
theta.hat[i] <- mean(40*I(x))
}
plot(theta.hat, main = "Monte Carlo",ylab = "I_hat")
Mc.mean <- mean(theta.hat)
Mc.var <- var(theta.hat)

# (2) Importance Sampling Monte Carlo
# N(0,1)

theta.hat <- NULL

for( i in 1:N){
x <- rnorm(n,0,1)
theta.hat[i] <- mean(40*I(x)*dunif(x,-20,20)/dnorm(x,0,1))

}
plot(theta.hat, main = "Importance Sampling Monte Carlo\n N(0,1)",ylab = "I_hat")
t1 <- theta.hat[-which(theta.hat>3)]
plot(t1, main = "Importance Sampling Monte Carlo\n N(0,1)",ylab = "I_hat")
N1.mean <- mean(theta.hat)
N1.var <- var(theta.hat)

# (3) Importance Sampling Monte Carlo
# N(0,5)

theta.hat <- NULL
for( i in 1:N){
  x <- rnorm(n,0,20)
  theta.hat[i] <- mean(40*I(x)*dunif(x,-20,20)/dnorm(x,0,20))
}
plot(theta.hat, main = "Importance Sampling Monte Carlo\n N(0,5)",ylab = "I_hat")

N2.mean <- mean(theta.hat)
N2.var <- var(theta.hat)

# Compare three methods
Q1M <- cbind(Mc.mean, N1.mean, N2.mean) %>% round(., digits = 5)
Q1V <- cbind(Mc.var, N1.var, N2.var) %>% round(., digits = 5)
Q1 <- rbind(Q1M,Q1V)
rownames(Q1) <- c("Mean", "Variance")
colnames(Q1) <- c("MC", "N(0,1)", "N(0,5)")
Q1






# Question 02 -------------------------------------------------------------

# Find N(0,1) mean and variance by important sampling with t3
ME <- function(x){
  1/sqrt((2*pi))*exp(-x^2/2)
}

# Mean
theta.hat <- NULL
for(i in 1:N){
x <- rt(n,3)
theta.hat[i] <- mean(x*ME(x)*dunif(x,-30,30)/dt(x,3)*60)
}
Me.mean <- mean(theta.hat)
Me.var <- var(theta.hat)

# Variance
theta.hat1 <- NULL
theta.hat2 <- NULL
theta.hat <- NULL
for(i in 1:N){
  x <- rt(n,3)
  theta.hat1 <- mean(x^2*ME(x)*dunif(x,-30,30)/dt(x,3)*60)
  theta.hat2 <- mean(x*ME(x)*dunif(x,-30,30)/dt(x,3)*60)
  theta.hat[i] <- theta.hat1 - (theta.hat2)^2
}
Va.mean <- mean(theta.hat)
Va.var <- var(theta.hat)

# Find t3 mean and variance by important sampling with N(0,1)
v <- 3
tv <- function(x,v){
  gamma((v+1)/2)/sqrt(v*pi)/gamma(v/2)/(1+x^2/v)^(v+1)/2
}

theta.hat <- NULL
for(i in 1:N){
x <- rnorm(n,0,1)
theta.hat[i] <- mean(x*tv(x,v)*dunif(x,-30,30)/dnorm(x,0,1)*60)
}
mean(theta.hat)
var(theta.hat)

theta.hat <- NULL
for(i in 1:N){
  x <- rnorm(n,0,1)
  theta.hat1 <- (mean(x*tv(x,v)*dunif(x,-30,30)/dnorm(x,0,1)*60))^2
  theta.hat2 <- mean(x^2*tv(x,v)*dunif(x,-30,30)/dnorm(x,0,1)*60)
  theta.hat[i] <- theta.hat2 - theta.hat1
}
mean(theta.hat)
var(theta.hat)



