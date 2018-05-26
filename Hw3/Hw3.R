library(ggplot2)
# x_old = -10 -------------------------------------------------------------
# s = 0.1 -----------------------------------------------------------------

set.seed (2018);
x=x_old=-10; x_new=0; s = 0.1; k=500; accept = 0;minn = NULL
for (i in 1:k){
  y = x_old+ s*rnorm(1,0,1)
  alpha = exp(-(y^2-(x_old)^2)/2)
  mina = min(alpha,1)
  minn <- c(minn,mina)
  x_new = sample(c(x_old,y),1,prob = c(1-mina,mina))
  accept <- ifelse(x_new == y , accept + 1, accept + 0 )
  x = c(x,x_new);
  x_old = x_new;
}

acc_rate=accept/k
acc_rate
plot(x, type = "l")
acf(x)

# ggplot
temp <- data.frame(index = 1:(k+1), x_new = x)
ggplot(data = temp)+
  geom_line(mapping = aes(x = index, y = x_new))+
  ylab("xt")+
  xlab("t")

ggplot(data = temp, mapping = aes(x = index, y = x_new)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = index, yend = 0))

# s = 0.5 -----------------------------------------------------------------

set.seed (2018);
x=x_old=-10; x_new=0; s = 0.5; k=500; accept = 0;minn = NULL
for (i in 1:k){
  y = x_old+ s*rnorm(1,0,1)
  alpha = exp(-(y^2-(x_old)^2)/2)
  mina = min(alpha,1)
  minn <- c(minn,mina)
  x_new = sample(c(x_old,y),1,prob = c(1-mina,mina))
  accept <- ifelse(x_new == y , accept + 1, accept + 0 )
  x = c(x,x_new);
  x_old = x_new;
}

acc_rate=accept/k
acc_rate
plot(x, type = "l")
acf(x)

# ggplot
temp <- data.frame(index = 1:(k+1), x_new = x)
ggplot(data = temp)+
  geom_line(mapping = aes(x = index, y = x_new))+
  ylab("xt")+
  xlab("t")

ggplot(data = temp, mapping = aes(x = index, y = x_new)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = index, yend = 0))


# s = 10 ------------------------------------------------------------------

set.seed (2018);
x=x_old=-10; x_new=0; s = 10; k=500; accept = 0;minn = NULL
for (i in 1:k){
  y = x_old+ s*rnorm(1,0,1)
  alpha = exp(-(y^2-(x_old)^2)/2)
  mina = min(alpha,1)
  minn <- c(minn,mina)
  x_new = sample(c(x_old,y),1,prob = c(1-mina,mina))
  accept <- ifelse(x_new == y , accept + 1, accept + 0 )
  x = c(x,x_new);
  x_old = x_new;
}
acc_rate=accept/k
acc_rate
plot(x, type = "l")
acf(x)

# ggplot
temp <- data.frame(index = 1:(k+1), x_new = x)
ggplot(data = temp)+
  geom_line(mapping = aes(x = index, y = x_new))+
  ylab("xt")+
  xlab("t")

ggplot(data = temp, mapping = aes(x = index, y = x_new)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = index, yend = 0))


# x_old = 0 -------------------------------------------------------------
# s = 0.1 -----------------------------------------------------------------

set.seed (2018);
x=x_old= 0; x_new=0; s = 0.1; k=10000; accept = 0;minn = NULL
for (i in 1:k){
  y = x_old+ s*rnorm(1,0,1)
  alpha = exp(-(y^2-(x_old)^2)/2)
  mina = min(alpha,1)
  minn <- c(minn,mina)
  x_new = sample(c(x_old,y),1,prob = c(1-mina,mina))
  accept <- ifelse(x_new == y , accept + 1, accept + 0 )
  x = c(x,x_new);
  x_old = x_new;
}

acc_rate=accept/k
acc_rate
plot(x, type = "l")
acf(x)

# ggplot
temp <- data.frame(index = 1:(k+1), x_new = x)
ggplot(data = temp)+
  geom_line(mapping = aes(x = index, y = x_new))+
  ylab("xt")+
  xlab("t")

ggplot(data = temp, mapping = aes(x = index, y = x_new)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = index, yend = 0))

# s = 0.5 -----------------------------------------------------------------

set.seed (2018);
x=x_old= 0; x_new=0; s = 0.5; k=1000; accept = 0;minn = NULL
for (i in 1:k){
  y = x_old+ s*rnorm(1,0,1)
  alpha = exp(-(y^2-(x_old)^2)/2)
  mina = min(alpha,1)
  minn <- c(minn,mina)
  x_new = sample(c(x_old,y),1,prob = c(1-mina,mina))
  accept <- ifelse(x_new == y , accept + 1, accept + 0 )
  x = c(x,x_new);
  x_old = x_new;
}

acc_rate=accept/k
acc_rate
plot(x, type = "l")
acf(x)

# ggplot
temp <- data.frame(index = 1:(k+1), x_new = x)
ggplot(data = temp)+
  geom_line(mapping = aes(x = index, y = x_new))+
  ylab("xt")+
  xlab("t")

ggplot(data = temp, mapping = aes(x = index, y = x_new)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = index, yend = 0))

# s = 10 ------------------------------------------------------------------

set.seed (2018);
x=x_old= 0; x_new=0; s = 10; k=500; accept = 0;minn = NULL
for (i in 1:k){
  y = x_old+ s*rnorm(1,0,1)
  alpha = exp(-(y^2-(x_old)^2)/2)
  mina = min(alpha,1)
  minn <- c(minn,mina)
  x_new = sample(c(x_old,y),1,prob = c(1-mina,mina))
  accept <- ifelse(x_new == y , accept + 1, accept + 0 )
  x = c(x,x_new);
  x_old = x_new;
}
acc_rate=accept/k
acc_rate
plot(x, type = "l")
acf(x)

# ggplot
temp <- data.frame(index = 1:(k+1), x_new = x)
ggplot(data = temp)+
  geom_line(mapping = aes(x = index, y = x_new))+
  ylab("xt")+
  xlab("t")

ggplot(data = temp, mapping = aes(x = index, y = x_new)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = index, yend = 0))
# x_old = -10 -------------------------------------------------------------
# s = 3 -------------------------------------------------------------------

set.seed (2018);
x=x_old=-10; x_new=0; s = 3; k=500; accept = 0;minn = NULL
for (i in 1:k){
  y = x_old+ s*rnorm(1,0,1)
  alpha = exp(-(y^2-(x_old)^2)/2)
  mina = min(alpha,1)
  minn <- c(minn,mina)
  x_new = sample(c(x_old,y),1,prob = c(1-mina,mina))
  accept <- ifelse(x_new == y , accept + 1, accept + 0 )
  x = c(x,x_new);
  x_old = x_new;
}

acc_rate=accept/k
acc_rate
plot(x, type = "l")
acf(x)

# ggplot
temp <- data.frame(index = 1:(k+1), x_new = x)
ggplot(data = temp)+
  geom_line(mapping = aes(x = index, y = x_new))+
  ylab("xt")+
  xlab("t")

ggplot(data = temp, mapping = aes(x = index, y = x_new)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = index, yend = 0))
# x_old = 0 ---------------------------------------------------------------
# s = 0.1 -----------------------------------------------------------------

set.seed (2018);
x=x_old=0; x_new=0; s = 3; k=500; accept = 0;minn = NULL
for (i in 1:k){
  y = x_old+ s*rnorm(1,0,1)
  alpha = exp(-(y^2-(x_old)^2)/2)
  mina = min(alpha,1)
  minn <- c(minn,mina)
  x_new = sample(c(x_old,y),1,prob = c(1-mina,mina))
  accept <- ifelse(x_new == y , accept + 1, accept + 0 )
  x = c(x,x_new);
  x_old = x_new;
}

acc_rate=accept/k
acc_rate
plot(x, type = "l")
acf(x)

# ggplot
temp <- data.frame(index = 1:(k+1), x_new = x)
ggplot(data = temp)+
  geom_line(mapping = aes(x = index, y = x_new))+
  ylab("xt")+
  xlab("t")

ggplot(data = temp, mapping = aes(x = index, y = x_new)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = index, yend = 0))
