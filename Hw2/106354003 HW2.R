library(mvtnorm)
library(dplyr)
library(ggplot2)


# EM ----------------------------------------------------------------------


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


End_theta <- EM_faithful(alpha, mu1, mu2, sigma1, sigma2, num)


# Plot --------------------------------------------------------------------
# refer from other code
xpts <- seq(from=1,to=6,length.out=100)
ypts <- seq(from=40,to=100,length.out=100)

#function to plot current data
plot.em <- function(theta){
  mixture.contour <- outer(xpts,ypts,function(x,y) {
    theta$tau[1]*dmvnorm(cbind(x,y),mean=theta$mu1,sigma=theta$sigma1) + theta$tau[2]*dmvnorm(cbind(x,y),mean=theta$mu2,sigma=theta$sigma2)
  })
  contour(xpts,ypts,mixture.contour,nlevels=5,drawlabel=FALSE,col="red",xlab="Eruption time",ylab="Waiting time",main="Waiting time vs Eruption time of the Old Faithful geyser")
  points(faithful)
}

plot.em(End_theta)


# Clustering --------------------------------------------------------------

a <- dmvnorm(faithful,End_theta$mu1,End_theta$sigma1  )
b <- dmvnorm(faithful,End_theta$mu2,End_theta$sigma2  )
Normal.dist. <- ifelse(a>b, "First","Second")

temp <- data.frame(a = faithful$waiting, b = faithful$eruptions, c = c)

ggplot(temp)+
  geom_point(mapping = aes(x = a, y = b,color = Normal.dist.))+
  xlab("Waiting")+
  ylab("Eruptions")
