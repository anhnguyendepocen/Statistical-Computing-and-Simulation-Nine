library(ggplot2)
library(kableExtra)
library(dplyr)
library(magrittr)
options(scipen = 10)
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
set.seed(3)
# Find N(0,1) mean and variance by important sampling with t3
ME <- function(x){
  1/sqrt((2*pi))*exp(-x^2/2)
}
N = 5000
# Mean
# diffn
diffn_mean <- NULL
diffn_var <- NULL
n <- seq(100,1000,by = 100)
for(j in 1:length(n)){
theta.hat <- NULL
for(i in 1:N){
x <- rt(n[j],3)
theta.hat[i] <- mean(x*ME(x)*dunif(x,-30,30)/dt(x,3)*60)
}
diffn_mean <- c(diffn_mean,mean(theta.hat))
diffn_var <- c(diffn_var,var(theta.hat))
}

diffn <- data.frame(c(1:10),diffn_mean, diffn_var) %>% round(., digits = 5)
t(diffn)
# 繪圖
# mean
ggplot(diffn) + 
  geom_point(aes(x = diffn$c.1.10., y = diffn_mean)) +
  geom_smooth(aes(x = diffn$c.1.10., y = diffn_mean), se = FALSE) +
  labs(title="N(0,1) mean of important sampling with t(3)\n Mean",
       x="Index",
       y="Mean of estimator of mean") +
  theme_bw() 

# Variance
ggplot(diffn) + 
  geom_point(aes(x = diffn$c.1.10., y = diffn_var)) +
  geom_smooth(aes(x = diffn$c.1.10., y = diffn_var),se = FALSE) +
  labs(title="N(0,1) mean of important sampling with t(3)\n Variance",
       x="Index",
       y="Variance of estimator of mean") +
  theme_bw() 

# Variance
diffn_mean <- NULL
diffn_var <- NULL
n <- seq(100,1000,by = 100)
for(j in 1:length(n)){
theta.hat <- NULL
for(i in 1:N){
  x <- rt(n[j],3)
  theta.hat1 <- mean(x^2*ME(x)*dunif(x,-30,30)/dt(x,3)*60)
  theta.hat2 <- mean(x*ME(x)*dunif(x,-30,30)/dt(x,3)*60)
  theta.hat[i] <- theta.hat1 - (theta.hat2)^2
}
diffn_mean <- c(diffn_mean,mean(theta.hat))
diffn_var <- c(diffn_var,var(theta.hat))
}

diffn <- data.frame(c(1:10),diffn_mean, diffn_var) %>% round(., digits = 5)
t(diffn)
# 繪圖
# mean
ggplot(diffn) + 
  geom_point(aes(x = diffn$c.1.10., y = diffn_mean)) +
  geom_smooth(aes(x = diffn$c.1.10., y = diffn_mean), se = FALSE) +
  labs(title="N(0,1) variance of important sampling with t(3)\n Mean",
       x="Index",
       y="Mean of estimator of variance") +
  theme_bw() 

# Variance
ggplot(diffn) + 
  geom_point(aes(x = diffn$c.1.10., y = diffn_var)) +
  geom_smooth(aes(x = diffn$c.1.10., y = diffn_var), se = FALSE) +
  labs(title="N(0,1) variance of important sampling with t(3)\n Variance",
       x="Index",
       y="Variance of estimator of variance") +
  theme_bw() 


# Find t3 mean and variance by important sampling with N(0,1)
set.seed(3)
v <- 3
tv <- function(x,v){
  z <- gamma((v+1)/2)/sqrt(v*pi)/gamma(v/2)/(1+(x^2)/v)^((v+1)/2)
  return(z)
}

# mean
diffn_mean <- NULL
diffn_var <- NULL
n <- seq(100,1000,by = 100)
for(j in 1:length(n)){
theta.hat <- NULL
for(i in 1:N){
x <- rnorm(n[j],0,1)
theta.hat[i] <- mean(x*tv(x,v)/dnorm(x,0,1))
}
diffn_mean <- c(diffn_mean,mean(theta.hat))
diffn_var <- c(diffn_var,var(theta.hat))
}
diffn <- data.frame(c(1:10),diffn_mean, diffn_var) %>% round(., digits = 5)
t(diffn)
# 繪圖

# mean
ggplot(diffn) + 
  geom_point(aes(x = diffn$c.1.10., y = diffn_mean)) +
  geom_smooth(aes(x = diffn$c.1.10., y = diffn_mean), se = FALSE) +
  labs(title="t(3) mean of important sampling with N(0,1)\n Mean",
       x="Index",
       y="mean of estimator of mean") +
  theme_bw() 

# Variance
ggplot(diffn) + 
  geom_point(aes(x = diffn$c.1.10., y = diffn_var)) +
  geom_smooth(aes(x = diffn$c.1.10., y = diffn_var), se = FALSE) +
  labs(title="t(3) mean of important sampling with N(0,1)\n Variance",
       x="Index",
       y="Variance of estimator of mean") +
  theme_bw() 



# Var
diffn_mean <- NULL
diffn_var <- NULL
n <- seq(100,1000,by = 100)
for(j in 1:length(n)){
theta.hat <- NULL
for(i in 1:N){
  x <- rnorm(n[j],0,1)
  theta.hat1 <- (mean(x*tv(x,v)/dnorm(x,0,1)))^2
  theta.hat2 <- mean((x^2)*tv(x,v)/dnorm(x,0,1))
  theta.hat[i] <- theta.hat2 - theta.hat1
}
diffn_mean <- c(diffn_mean,mean(theta.hat))
diffn_var <- c(diffn_var,var(theta.hat))
}

diffn <- data.frame(c(1:10),diffn_mean, diffn_var) %>% round(., digits = 5)
t(diffn)
# 繪圖

# mean
ggplot(diffn) + 
  geom_point(aes(x = diffn$c.1.10., y = diffn_mean)) +
  geom_smooth(aes(x = diffn$c.1.10., y = diffn_mean), se = FALSE) +
  labs(title="t(3) Variance of important sampling with N(0,1)\n Mean",
       x="Index",
       y="mean of estimator of variance") +
  theme_bw() 

# Variance
ggplot(diffn) + 
  geom_point(aes(x = diffn$c.1.10., y = diffn_var)) +
  geom_smooth(aes(x = diffn$c.1.10., y = diffn_var), se = FALSE) +
  labs(title="t(3) Variance of important sampling with N(0,1)\n Variance",
       x="Index",
       y="Variance of estimator of variance") +
  theme_bw() 

f <- function(x){
  (1/40)/dnorm(x,0,5)
}

curve(f,-20,20,ylab="f(x)/g(x)")
curve(dnorm(x),5,10,col = "red",ylab = "p", add = T,ylim = c(0,0.004))
curve(dt(x,3),5,10,col = "blue",ylab = "p",ylim = c(0,0.004), main = "t(3)/ N(0,1)")
