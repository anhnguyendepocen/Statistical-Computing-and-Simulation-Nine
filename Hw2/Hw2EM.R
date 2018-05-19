library(mvtnorm)
library(dplyr)

head(faithful)


plot(faithful$eruptions, faithful$waiting, xlab = "Eruptions", ylab = "Waiting")

alpha <- 0.5
mu1 <- c(4,60)
mu2 <- c(3,70)
sigma1 <-matrix(c(1,0,0,1),2,2)
sigma2 <- matrix(c(1,0,0,1),2,2)
num = 10

EM_faithful <- function(alpha, mu1, mu2, sigma1, sigma2, num){
newalpha <- NULL
newmu1 <- NULL
newmu2 <- NULL
newsigma1 <- NULL
newsigma2 <- NULL

for(j in 1:num){
p = matrix(0,nrow(faithful),2)
temp_mu1= matrix(0,nrow(faithful),2)
temp_mu2= matrix(0,nrow(faithful),2)
temp_sigma1= matrix(0,nrow(faithful),4)
temp_sigma2= matrix(0,nrow(faithful),4)
for( i in 1:nrow(faithful)){
  x <- faithful[i,] %>% as.numeric()
p[i,1] <- alpha*dmvnorm(x,mu1,sigma1)/ (alpha*dmvnorm(x,mu1,sigma1)+(1 - alpha)*dmvnorm(x,mu2,sigma2))
temp_mu1[i,] <- x*p[i,1]
temp_sigma1[i,] <- (x - mu1)%*%t(x - mu1)*p[i,1]
p[i,2] <- (1-alpha)*dmvnorm(x,mu2,sigma2)/ (alpha*dmvnorm(x,mu1,sigma1)+(1 - alpha)*dmvnorm(x,mu2,sigma2))
temp_mu2[i,] <- x*p[i,2]
temp_sigma2[i,] <- (x - mu2)%*%t(x - mu2)*p[i,2]}

newalpha <- c(newalpha, mean(p[,1]))
alpha <- mean(p[,1])
newmu1 <- c(newmu1, apply(temp_mu1,2,sum)/sum(p[,1]))
mu1 <- apply(temp_mu1,2,sum)/sum(p[,1]) 
newmu2 <- c(newmu2, apply(temp_mu2,2,sum)/sum(p[,2]))
mu2 <- apply(temp_mu2,2,sum)/sum(p[,2]) 
newsigma1<- c(newsigma1, apply(temp_sigma1,2,sum)/sum(p[,1]))
sigma1 <- matrix(apply(temp_sigma1,2,sum)/sum(p[,1]),2,2)
newsigma2<- c(newsigma2, apply(temp_sigma2,2,sum)/sum(p[,2]))
sigma2 <- matrix(apply(temp_sigma2,2,sum)/sum(p[,2]) ,2,2)
}
 
names(mu1) <- c("eruptions", "waiting")
names(mu2) <- c("eruptions", "waiting")
temp <- list(c(alpha,1-alpha), mu1, sigma1, mu2, sigma2)
names(temp) <- c("tau", "mu1", "sigma1", "mu2", "sigma2")

return(temp)
}

EM_faithful(alpha, mu1, mu2, sigma1, sigma2, num)


matrix(newmu2,2)


theta = list(
  tau <- c(0.5,0.5),
  mu1 <- c(4,60),
  mu2 <- c(3,70),
  sigma1 <-matrix(c(1,0,0,1),2,2),
  sigma2 <- matrix(c(1,0,0,1),2,2),
  num = 10)
plot.em(theta = theta)

EM_faithful(alpha, mu1, mu2, sigma1, sigma2, num)



p1 <- alpha*dmvnorm(faithful, mu1, sigma1)/ (alpha*dmvnorm(faithful,mu1,sigma1)+(1 - alpha)*dmvnorm(faithful,mu2,sigma2))
p2 <- (1-alpha)*dmvnorm(faithful,mu2,sigma2)/ (alpha*dmvnorm(faithful,mu1,sigma1)+(1 - alpha)*dmvnorm(faithful,mu2,sigma2))

temp_mu1 <- faithful*p1
temp_mu2 <- faithful*p2

apply(faithful - mu1,1,function(x) x%*%t(x)*p1)
faithful - mu2
newalpha <- c(newalpha, mean(p1))
alpha <- mean(p1)
newmu1 <- c(newmu1, apply(faithful*p1,2,sum)/sum(p1))
mu1 <- apply(faithful*p1,2,sum)/sum(p1)
newmu2 <- c(newmu2, apply(faithful*p2,2,sum)/sum(p2))
mu2 <- apply(faithful*p2,2,sum)/sum(p2)
newsigma1<- c(newsigma1, apply(temp_sigma1,2,sum)/sum(p1))
sigma1 <- matrix(apply(temp_sigma1,2,sum)/sum(p1),2,2)
newsigma2<- c(newsigma2, apply(temp_sigma2,2,sum)/sum(p2))
sigma2 <- matrix(apply(temp_sigma2,2,sum)/sum(p2) ,2,2)


a <- matrix(c(1,2,3,4),2,2)
apply(a,1,function(x),)