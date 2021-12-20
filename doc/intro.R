## -----------------------------------------------------------------------------
x = rnorm(10)
print(x)

## -----------------------------------------------------------------------------
x <- seq(-5,5,length.out=100)
plot(x,dnorm(x,0,0.5),type='l')
lines(x,dnorm(x,0,1))
lines(x,dnorm(x,0,2))

## -----------------------------------------------------------------------------
num <- c(1:3)
name <- c('张三','李四','王五')
sex <- c('女','男','女')
x <- data.frame(num,name,sex);x

## -----------------------------------------------------------------------------
n <- 500
u <- runif(n)
sigma <- runif(1, 0, 1000)
x <- sqrt(-2 * sigma^2 * log(1 - u))
hist(x, prob = TRUE, main=expression("f(x)==x/(sigma^2)*exp(-x^2/(2*(sigma^2)))"))
y <- seq(0, 10000, 0.01)
lines(y, y/sigma^2*exp(-y^2/(2*sigma^2)))

## -----------------------------------------------------------------------------
n <- 10000
X1 <- rnorm(n, 0, 1)
X2 <- rnorm(n, 3, 1)
r <- sample(c(0, 1), n, replace = TRUE)
Z <- r * X1 + (1 - r) * X2
hist(Z)
p1 <- 0.75
p2 <- 0.1
p3 <- 0.2
p4 <- 0.3
p5 <- 0.4
p6 <- 0.5
p7 <- 0.6
p8 <- 0.7
p9 <- 0.8
p10 <- 0.9
Z1 <- p1 * X1 + (1 - p1) * X2
Z2 <- p2 * X1 + (1 - p2) * X2
Z3 <- p3 * X1 + (1 - p3) * X2
Z4 <- p4 * X1 + (1 - p4) * X2
Z5 <- p5 * X1 + (1 - p5) * X2
Z6 <- p6 * X1 + (1 - p6) * X2
Z7 <- p7 * X1 + (1 - p7) * X2
Z8 <- p8 * X1 + (1 - p8) * X2
Z9 <- p9 * X1 + (1 - p9) * X2
Z10 <- p10 * X1 + (1 - p10) * X2
hist(Z1); hist(Z2); hist(Z3); hist(Z4); hist(Z5); hist(Z6); hist(Z7); hist(Z8); hist(Z9); hist(Z10)

## -----------------------------------------------------------------------------
n <- 1000
p <- rpois(n, 2)
x <- rgamma(n, 1*p, 2)
# 以上为随机生成n个复合分布的数字

#下计算x(10)
q <- rpois(n, 10*2)
y <- rgamma(n, q*1, 2)
print(mean(x))
print(var(x))

## -----------------------------------------------------------------------------
n <- 1000
y <- c()
for (j in (1:1000)){
    x <- rgamma(rpois(1,2),1,2)
    #print(j)
    y[j] <- sum(x)
    #print(sum(x))
}
print(mean(y))
print(var(y))

## -----------------------------------------------------------------------------
fx <- function(i){
  m <- 5000
  x <- runif(m,0,i)
  theta <- mean(30*x^2*(1-x)^2*i)
  print(c(i,theta,pbeta(i,3,3)))
}
for (j in seq(0.1,0.9,0.1)){
  fx(j)
}

## -----------------------------------------------------------------------------
fx <- function(x,r=1000,antithetic=TRUE){
  u <- runif(r/2)
  if (! antithetic) v <- runif(r/2) else
    v <- 1-u
  u <- c(u,v)
  cdf <- numeric(length(x))
  for (i in 1:length(x)){
    g <- x[i]^2*u*exp(-(x[i]*u)^2/2)
    cdf[i] <- mean(g)
  }
  cdf
}

x <- seq(1,100,20)
set.seed(123)
fx1 <- fx(x,antithetic=FALSE)
set.seed(123)
fx2 <- fx(x,antithetic=TRUE)
print(round(rbind(x,fx1,fx2),5))


## -----------------------------------------------------------------------------
n <- 5000
g <- function(x){
  x^2 * exp(-x^2/2) / sqrt(2*pi) * (x > 1)
}
hat <- se <- numeric(2)


x <- rexp(n, 1)+1
fg <- g(x) / exp(1-x)
hat[1] <- mean(fg)
se[1] <- sd(fg)


x <- rgamma(n,2,2)+1
fg <- g(x) / (4*(x-1)*exp(-2*(x-1)))
hat[2] <- mean(fg)
se[2] <- sd(fg)



rbind(hat, se)

## -----------------------------------------------------------------------------
n <- 5000
g <- function(x){
  x^2 * exp(-x^2/2) / sqrt(2*pi) * (x > 1)
}
x <- rexp(n, 1)+1
fg <- g(x) / exp(1-x)
hat <- mean(fg)
se <- sd(fg)
print(hat)
print(se)

## -----------------------------------------------------------------------------
#设置一些必要参数
UCL <- numeric(1000)
P <- numeric(2)
n <- 20
alpha <- 0.05

# 正态分布的部分
for (i in 1:1000){
  x <- rnorm(n, mean=0, sd=2)
  UCL[i] <- (n-1) * var(x) / qchisq(alpha, df=n-1)
}
P[1] <- mean(UCL > 4)

# 卡方分布的部分
for (i in 1:1000){
  x <- rchisq(n, df=2)
  UCL[i] <- (mean(x)-2)*sqrt(n)/sd(x)
}
P[2] <- mean(abs(UCL) < qt(1-alpha,n-1))

# 输出表格
d <- data.frame(P,row.names=c("norm","chisq"))
knitr::kable(d)

## -----------------------------------------------------------------------------
# 卡方(1)
n <- 20
alpha <- 0.05
mu0 <- 1

m <- 1000
p <- numeric(m)

for (j in 1:m){
  x <- rchisq(n, 1)
  ttest <- t.test(x, alternative = "two.sided", mu = mu0)
  p[j] <- ttest$p.value
}
p.hat <- mean(p < alpha)
se.hat <- sqrt(p.hat * (1 - p.hat) / m)
print(c(p.hat, se.hat))


# U(0,2)
for (j in 1:m){
  x <- runif(n, 0, 2)
  ttest <- t.test(x, alternative = "two.sided", mu = mu0)
  p[j] <- ttest$p.value
}
p.hat <- mean(p < alpha)
se.hat <- sqrt(p.hat * (1 - p.hat) / m)
print(c(p.hat, se.hat))

# 指数(1)
for (j in 1:m){
  x <- rexp(n, 1)
  ttest <- t.test(x, alternative = "two.sided", mu = mu0)
  p[j] <- ttest$p.value
}
p.hat <- mean(p < alpha)
se.hat <- sqrt(p.hat * (1 - p.hat) / m)
print(c(p.hat, se.hat))

## -----------------------------------------------------------------------------
library(MASS) # 调用此包用于产生多元正态分布的随机数
func <- function(data){
  n <- nrow(data) #数据的行数
  c <- ncol(data) #数据的列数
  central <- data #数据本身，以方便下做变换
  for (i in 1:c){
    central[,i] <- data[,i]-mean(data[,i]) #对原数据进行变体
  }
  sigma <- t(central)%*%central/n #计算统计量1/4
  a <- central%*%solve(sigma)%*%t(central) #计算统计量2/4
  b <- sum(colSums(a^3))/(n^2) #计算统计量3/4
  test <- n*b/6 #计算统计量4/4
  chi <- qchisq(0.95, c*(c+1)*(c+2)/6) #边界值
  as.integer(test>chi) #比较
}


mu <- c(0,0) #均值向量
sigma <- matrix(c(1,0,0,1),nrow=2,ncol=2) #方差矩阵
m <- 100 #运行次数
n <- c(10,20,30,50,100,200,300,400,500) #不同的样本量
a <- numeric(length(n)) #长度为9
for (i in 1:length(n)){
  a[i] = mean(replicate(m, expr={
    data <- mvrnorm(n[i],mu,sigma)
    func(data)
  }))
}
print(a)

## -----------------------------------------------------------------------------
mu1 <- mu2 <- c(0,0)
sigma1 <- matrix(c(1,0,0,1),nrow=2,ncol=2)
sigma2 <- matrix(c(100,0,0,100),nrow=2,ncol=2)
sigma <- list(sigma1,sigma2)
m = 100
n = 100
epsilon <- c(seq(0,0.1,0.01),seq(0.1,1,0.05))
N <- length(epsilon)
pwr <- numeric(N)
for (j in 1:N){
  e <- epsilon[j]
  sktests <- numeric(m)
  for (i in 1:m){
    index=sample(c(1,2),replace=TRUE,size=n,prob=c(1-e,e))
    data <- matrix(0,nrow=n,ncol=2)
    for (t in 1:n){
      if(index[t]==1) data[t,]=mvrnorm(1,mu1,sigma1)
      else data[t,]=mvrnorm(1,mu2,sigma2)
    }
    sktests[i] <- func(data)
  }
  pwr[j] <- mean(sktests)
}
plot(epsilon,pwr,type = 'b', xlab = bquote(epsilon), ylim = c(0,1))
abline(h = 0.01, lty = 3)
se <- sqrt(pwr * (1 - pwr) / m)
lines(epsilon, pwr+se, lty = 3)
lines(epsilon, pwr-se, lty = 3)

## -----------------------------------------------------------------------------
library(bootstrap)
n <- nrow(scor)
lambda_hat <- eigen(cov(scor))$values
theta_hat <- lambda_hat[1] / sum(lambda_hat)
B <- 1000
theta <- c()

for (b in 1:B){
  scor1 <- sample(scor, size = n, replace = TRUE)
  lambda <- eigen(cov(scor1))$values
  theta[b] <- lambda[1] / sum(lambda)
}
bias_boot <- mean(theta) - theta_hat
se_boot <- sd(theta)
print(round(c('bias_boot'=bias_boot,'se_boot'=se_boot),4))

## -----------------------------------------------------------------------------
library(bootstrap)
set.seed(123)
n <- nrow(scor)
lambda_hat <- eigen(cov(scor))$values
theta_hat <- lambda_hat[1] / sum(lambda_hat)
theta_j <- c()

for (i in 1:n){
  x <- scor[-i,]
  lambda <- eigen(cov(x))$values
  theta_j[i] <- lambda[1]/sum(lambda)
  
}
bias_jack <- (n-1)*(mean(theta_j) - theta_hat)
se_jack <- (n-1)*sqrt(var(theta_j)/n)

print(round(c('bias_jack'=bias_jack,'se_jack'=se_jack),4))

## -----------------------------------------------------------------------------
# 95% percentile confidence intervals
library(boot)
theta.boot <- function(dat,i){
  lambda <- eigen(cov(dat[i,]))$values
  theta <- lambda[1] / sum(lambda)
  theta
}
dat <- scor
boot.obj <- boot(dat, statistic = theta.boot, R = 1000)
print(boot.obj)
alpha <- c(0.025, 0.975)
print(quantile(boot.obj$t, alpha, type=6))

## -----------------------------------------------------------------------------
# 95% BCa confidence intervals
library(boot)
theta.boot <- function(dat,i){
  lambda <- eigen(cov(dat[i,]))$values
  theta <- lambda[1] / sum(lambda)
  theta
}
dat <- scor
boot.obj <- boot(dat, statistic = theta.boot, R = 1000)
print(boot.ci(boot.obj, type=c("bca","perc")))

## -----------------------------------------------------------------------------
library(bootstrap)
library(boot)
n <- 100

x <- rnorm(n)
boot1 <- function(x,i){
  xm <- mean(x[i])
  m3 <- mean((x - xm)^3)
  m2 <- mean((x - xm)^2)
  skewness <- m3 / m2^1.5
}

m <- 100
p1 <- c()
p2 <- c()
p3 <- c()

for (i in 1:m){
  x <- rnorm(n)
  boot.obj <- boot(x, statistic = boot1, R=1000)
  a <- boot.ci(boot.obj,conf=0.95,type=c('basic','norm','perc'))
  p1[i] <- (a$norm[2]<0)*(0<a$norm[3])
  p2[i] <- (a$perc[4]<0)*(0<a$perc[5])
  p3[i] <- (a$basic[4]<0)*(0<a$basic[5])
}
mean(p1)
mean(p2)
mean(p3)

y <- rchisq(n,5)
boot2 <- function(y,i){
  ym <- mean(y[i])
  m3 <- mean((y - ym)^3)
  m2 <- mean((y - ym)^2)
  skewness <- m3 / m2^1.5
}
skew <-sqrt(8/5)
r1 <- c()
r2 <- c()
r3 <- c()
for (i in 1:m){
  y <- rchisq(n,5)
  boot.obj <- boot(y, statistic = boot2, R=1000)
  b <- boot.ci(boot.obj,conf=0.95,type=c('basic','norm','perc'))
  r1[i] <- (b$norm[2]<skew)*(skew<b$norm[3])
  r2[i] <- (b$perc[4]<skew)*(skew<b$perc[5])
  r3[i] <- (b$basic[4]<skew)*(skew<b$basic[5])
}
mean(r1)
mean(r2)
mean(r3)

## ----warning=F----------------------------------------------------------------
a <- c(12,54,13,45,32,2)
b <- c(1,78,28,46,69,46)
M <- 500
abc <- c(a, b)
gs <- 1:12
n<-length(a)
len <- numeric(M)
t0 <- (cor.test(a, b, method='spearman'))$estimate

for (i in 1:M) {
  gs <- sample(gs, size = n, replace = FALSE)
  a1 <- abc[gs]
  b1 <- abc[-gs]
  len[i] <- (cor.test(a1, b1,method='spearman'))$estimate
}

p <- mean(abs(c(t0, len)) >= abs(t0))
round(c(p,(cor.test(a, b, method='spearman'))$p.value),3)


## -----------------------------------------------------------------------------
library(RANN)
library(boot)
library(Ball)
library(energy)
library(MASS)

Function1 <- function(z, ix, sizes,k) {
  n1 <- sizes[1]
  n2 <- sizes[2]
  n <- n1 + n2
  z <- z[ix, ]
  NN <- nn2(data=z, k=k+1)
  
  x1 <- NN$nn.idx[1:n1,]
  x2 <- NN$nn.idx[(n1+1):n,]
  i1 <- sum(x1 < n1 + .5)
  i2 <- sum(x2 > n1+.5)
  (i1 + i2) / (k * n)
}

test.nn <- function(z,sizes,k){
  boot.obj <- boot(data=z, statistic=Function1, R=R, sim = "permutation", sizes=sizes, k=k)
  vec <- c(boot.obj$t0,boot.obj$t)
  p <- mean(vec >= vec[1])
  list(statistic=vec[1],p.value=p)
}

set.seed(1111)
u11 <- c(0,0)
sigma11 <- matrix(c(1,0,0,1),nrow=2,ncol=2)
u12 <- c(0,0)
sigma12 <- matrix(c(3,0,0,3),nrow=2,ncol=2)
n1 <- 28
n2 <- 28
n <- n1+n2 
N <- c(n1,n2)
k <- 2
R <- 99
m <- 88
alpha <- 0.05
pvalue <- matrix(NA,m,3)

for(i in 1:m){
  sj1 <- mvrnorm(n1,u11,sigma11)
  sj2 <- mvrnorm(n2,u12,sigma12)
  sj <- rbind(sj1,sj2)
  pvalue[i,1] <- test.nn(sj,N,k)$p.value
  pvalue[i,2] <- eqdist.etest(sj,sizes=N,R=R)$p.value
  pvalue[i,3] <- bd.test(x=sj1,y=sj2,num.permutations=R)$p.value
}
a1 <- mean(pvalue[,1]<alpha)
a2 <- mean(pvalue[,2]<alpha)
a3 <- mean(pvalue[,3]<alpha)
print(c(a1,a2,a3))

## -----------------------------------------------------------------------------
library(RANN)
library(boot)
library(Ball)
library(energy)
library(MASS)

Function1 <- function(z, ix, sizes,k) {
  n1 <- sizes[1]
  n2 <- sizes[2]
  n <- n1 + n2
  z <- z[ix, ]
  NN <- nn2(data=z, k=k+1)
  
  x1 <- NN$nn.idx[1:n1,]
  x2 <- NN$nn.idx[(n1+1):n,]
  i1 <- sum(x1 < n1 + .5)
  i2 <- sum(x2 > n1+.5)
  (i1 + i2) / (k * n)
}

test.nn <- function(z,sizes,k){
  boot.obj <- boot(data=z, statistic=Function1, R=R, sim = "permutation", sizes=sizes, k=k)
  vec <- c(boot.obj$t0,boot.obj$t)
  p <- mean(vec >= vec[1])
  list(statistic=vec[1],p.value=p)
}

u21 <- c(0,0)
sigma21 <- matrix(c(1,0,0,1),nrow=2,ncol=2)
u22 <- c(1,1)
sigma22 <- matrix(c(4,0,0,4),nrow=2,ncol=2)
n1 <- 28
n2 <- 28
n <- n1+n2 
N <- c(n1,n2)
k <- 2
R <- 99
m <- 88
alpha <- 0.05
pvalue <- matrix(NA,m,3)

for(i in 1:m){
  sj1 <- mvrnorm(n1,u21,sigma21)
  sj2 <- mvrnorm(n2,u22,sigma22)
  sj <- rbind(sj1,sj2)
  pvalue[i,1] <- test.nn(sj,N,k)$p.value
  pvalue[i,2] <- eqdist.etest(sj,sizes=N,R=R)$p.value
  pvalue[i,3] <- bd.test(x=sj1,y=sj2,num.permutations=R)$p.value
}
a1 <- mean(pvalue[,1]<alpha)
a2 <- mean(pvalue[,2]<alpha)
a3 <- mean(pvalue[,3]<alpha)
print(c(a1,a2,a3))

## -----------------------------------------------------------------------------
library(RANN)
library(boot)
library(Ball)
library(energy)
library(MASS)

Function1 <- function(z, ix, sizes,k) {
  n1 <- sizes[1]
  n2 <- sizes[2]
  n <- n1 + n2
  z <- z[ix, ]
  NN <- nn2(data=z, k=k+1)
  
  x1 <- NN$nn.idx[1:n1,]
  x2 <- NN$nn.idx[(n1+1):n,]
  i1 <- sum(x1 < n1 + .5)
  i2 <- sum(x2 > n1+.5)
  (i1 + i2) / (k * n)
}

test.nn <- function(z,sizes,k){
  boot.obj <- boot(data=z, statistic=Function1, R=R, sim = "permutation", sizes=sizes, k=k)
  vec <- c(boot.obj$t0,boot.obj$t)
  p <- mean(vec >= vec[1])
  list(statistic=vec[1],p.value=p)
}


n1 <- 28
n2 <- 28
n <- n1+n2 
N <- c(n1,n2)
k <- 2
R <- 99
m <- 88
alpha <- 0.05

p_value <- matrix(NA,m,3)
for(i in 1:m){
  sj1 <- as.matrix(rt(n1,1,3),ncol=1)
  sj2 <- as.matrix(rt(n2,1,6),ncol=1)
  sj <- rbind(sj1,sj2)
  p_value[i,1] <- test.nn(sj,N,k)$p.value
  p_value[i,2] <- eqdist.etest(sj,sizes=N,R=R)$p.value
  p_value[i,3] <- bd.test(x=sj1,y=sj2,num.permutations=R)$p.value
}
a1 <- mean(p_value[,1]<alpha)
a2 <- mean(p_value[,2]<alpha)
a3 <- mean(p_value[,3]<alpha)
print(c(a1,a2,a3))

## -----------------------------------------------------------------------------
library(RANN)
library(boot)
library(Ball)
library(energy)
library(MASS)

Function1 <- function(z, ix, sizes,k) {
  n1 <- sizes[1]
  n2 <- sizes[2]
  n <- n1 + n2
  z <- z[ix, ]
  NN <- nn2(data=z, k=k+1)
  
  x1 <- NN$nn.idx[1:n1,]
  x2 <- NN$nn.idx[(n1+1):n,]
  i1 <- sum(x1 < n1 + .5)
  i2 <- sum(x2 > n1+.5)
  (i1 + i2) / (k * n)
}

test.nn <- function(z,sizes,k){
  boot.obj <- boot(data=z, statistic=Function1, R=R, sim = "permutation", sizes=sizes, k=k)
  vec <- c(boot.obj$t0,boot.obj$t)
  p <- mean(vec >= vec[1])
  list(statistic=vec[1],p.value=p)
}

n41 <- 18
n42 <- 18
n43 <- 28
n44 <- 28
n1 <- n41
n2 <- n43
n <- n1+n2
N <- c(n1,n2)
k <- 2
R <- 99
m <- 88
alpha <- 0.05
pvalue <- matrix(NA,m,3)

u31 <- c(0,0)
sigma31 <- matrix(c(1,0,0,1),nrow=2,ncol=2)
u32 <- c(2,5)
sigma32 <- matrix(c(8,0,0,2),nrow=2,ncol=2)
u33 <- c(1,4)
sigma33 <- matrix(c(2,0,0,3),nrow=2,ncol=2)
u34 <- c(3,7)
sigma34 <- matrix(c(4,0,0,6),nrow=2,ncol=2)

for(i in 1:m){
  x1 <- mvrnorm(n41,u31,sigma31)
  x2 <- mvrnorm(n42,u32,sigma32)
  x3 <- mvrnorm(n43,u33,sigma33)
  x4 <- mvrnorm(n44,u34,sigma34)
  sj1 <- 0.3*x1 + 0.7*x2
  sj2 <- 0.4*x3 + 0.6*x4
  sj <- rbind(sj1,sj2)
  pvalue[i,1] <- test.nn(sj,N,k)$p.value
  pvalue[i,2] <- eqdist.etest(sj,sizes=N,R=R)$p.value
  pvalue[i,3] <- bd.test(x=sj1,y=sj2,num.permutations=R)$p.value
}
a1 <- mean(pvalue[,1]<alpha)
a2 <- mean(pvalue[,2]<alpha)
a3 <- mean(pvalue[,3]<alpha)
print(c(a1,a2,a3))

## -----------------------------------------------------------------------------
library(RANN)
library(boot)
library(Ball)
library(energy)
library(MASS)

Function1 <- function(z, ix, sizes,k) {
  n1 <- sizes[1]
  n2 <- sizes[2]
  n <- n1 + n2
  z <- z[ix, ]
  NN <- nn2(data=z, k=k+1)
  
  x1 <- NN$nn.idx[1:n1,]
  x2 <- NN$nn.idx[(n1+1):n,]
  i1 <- sum(x1 < n1 + .5)
  i2 <- sum(x2 > n1+.5)
  (i1 + i2) / (k * n)
}

test.nn <- function(z,sizes,k){
  boot.obj <- boot(data=z, statistic=Function1, R=R, sim = "permutation", sizes=sizes, k=k)
  vec <- c(boot.obj$t0,boot.obj$t)
  p <- mean(vec >= vec[1])
  list(statistic=vec[1],p.value=p)
}

u41 <- c(0,0)
sigma41 <- matrix(c(1,0,0,1),nrow=2,ncol=2)
u42 <- c(0,0)
sigma42 <- matrix(c(3,0,0,3),nrow=2,ncol=2)
n1 <- 38
n2 <- 28
n <- n1+n2 
N <- c(n1,n2)
k <- 2
R <- 99
m <- 88
alpha <- 0.05
pvalue <- matrix(NA,m,3)

for(i in 1:m){
  sj1 <- mvrnorm(n1,u41,sigma41)
  sj2 <- mvrnorm(n2,u42,sigma42)
  sj <- rbind(sj1,sj2)
  pvalue[i,1] <- test.nn(sj,N,k)$p.value
  pvalue[i,2] <- eqdist.etest(sj,sizes=N,R=R)$p.value
  pvalue[i,3] <- bd.test(x=sj1,y=sj2,num.permutations=R)$p.value
}
a1 <- mean(pvalue[,1]<alpha)
a2 <- mean(pvalue[,2]<alpha)
a3 <- mean(pvalue[,3]<alpha)
print(c(a1,a2,a3))

## -----------------------------------------------------------------------------
set.seed(1122)
f <- function(x,theta,eta){
  return (1 / (theta*pi*(1+((x-eta)/theta)^2)))
}

m <- 5000
theta <- 1
eta <- 0
x <- numeric(m)
x[1] <- rnorm(1,0,1)
k <- 0
u <- runif(m)

for (i in 2:m){
  xt <- x[i-1]
  y <- rnorm(1,xt,1)
  l <- f(y,theta,eta)*dnorm(xt,y,1)
  d <- f(xt,theta,eta)*dnorm(y,xt,1)
  if (u[i] < l/d) {
    x[i] <- y
    } else{
    x[i]<- xt
    k <- k+1
  }
}
print(k)

b <- 1001
y <- x[b:m]
a <- ppoints(20)
QR <- qcauchy(a)
Q <- quantile(x,a)

par(mfrow=c(1,2))
qqplot(QR,Q,main="",xlab="Cauchy Quantiles",ylab="Sample Quantiles")
abline(0,1)
hist(y,breaks="scott",main="",xlab="",freq=FALSE)
lines(QR,f(QR,theta,eta))

## ----warning=F----------------------------------------------------------------
N <- 500
burn <- 100
a <- 2
b <- 1
n <- 15

X <- matrix(0,N,2)
X[1,] <- c(0,1)

for (i in 2:N) {
  x2 <- X[i-1,2]
  X[i,1] <- rbinom(1, n, x2)
  x1 <- X[i,1]
  canshu1 <- x1+a
  canshu2 <- n-x1+b
  X[i,2] <- rbeta(1,canshu1,canshu2)
}

b <- burn+1
x <- X[b:N, ]
shuchu <- colMeans(x)
print(shuchu)

## ----warning=F----------------------------------------------------------------
Gelman.Rubin <- function(psi) {
  psi <- as.matrix(psi)
  n <- ncol(psi)
  k <- nrow(psi)
  psi.means <- rowMeans(psi)
  B <- n * var(psi.means)
  psi.w <- apply(psi,1,"var")
  W <- mean(psi.w)
  v.hat <- W*(n-1)/n+(B/n)
  r.hat <- v.hat / W
  return(r.hat)
}

#关于原函数
a <- 2
b <- 1
n <- 15

fxy <- function(n1,n2,N){
  X <- matrix(0,N,2)
  X[,1] <- n1
  X[,2] <- n2
  for (i in 2:N) {
    x2 <- X[i-1,2]
    X[i,1] <- rbinom(1, n, x2)
    x1 <- X[i,1]
    canshu1 <- x1+a
    canshu2 <- n-x1+b
    X[i,2] <- rbeta(1,canshu1,canshu2)
  }
  return(X)
}

n <- 1500
k <- 3
n1 <- c(2,4,6)
n2 <- c(1,2,3)
b <- 100
mu1 <- matrix(0,k,n)
mu2 <- matrix(0,k,n)

for (i in 1:k){
  h <- fxy(n1[i],n2[i],n)
  mu1[i,] <- h[,1]
  mu2[i,] <- h[,2]
}

psimu1 <- t(apply(mu1,1,cumsum))
psimu2 <- t(apply(mu1,1,cumsum))


## -----------------------------------------------------------------------------
fk <- function(k,d=2,x,y){
  return ((-1)^k * (x^2+y^2)^(k+1) * gamma((d+1)/2) * gamma(k + 1.5) / factorial(k) / 2^k / (2 * k + 1) / (2 * k+2) / gamma(k+d/2+1))
}

## -----------------------------------------------------------------------------
sum1 = 0
for (i in 0:99){
  sum1 = sum1 + fk(i,d=2, 1, 2)
}
print(sum1)

sum2 = 0
for (i in 0:119){
  sum2 = sum2 + fk(i,d=2, 1, 2)
}
print(sum2)

## -----------------------------------------------------------------------------
S <- function(a,k){
  ck <- sqrt(a^2*k/(k+1-a^2))
  pt(ck,df=k,lower.tail=FALSE)
}
f = function(a,k){S(a,k)-S(a,k-1)}

solve = function(k){
  output = uniroot(function(a){S(a,k)-S(a,k-1)},lower=0.1,upper=2)
  output$root
}
solve(100)
solve(200)
solve(300)
solve(500)
solve(1000)
solve(5000)

## -----------------------------------------------------------------------------
y <- c(0.21, 0.33, 0.43, 0.48, 0.54, 0.85, 0.91, 1, 1, 1)
EM <- function(y, max.it=1000, eps=1e-5){
  lambda = 0.5
  i <- 1
  lambda1 = 1
  lambda2 = 0.5
  while (abs(lambda1-lambda2) >= eps){
    lambda1 <- lambda2
    lambda2 <- (sum(y) + 3 * lambda2) / 10
    if (i == max.it) break
    i <- i + 1
  }
  return(lambda2)
}
EM(y,max.it=1000,eps=1e-5)

## -----------------------------------------------------------------------------
trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(100)
# 在源代码的基础上以向量矩阵输出
unlist(lapply(trims, function(trim) mean(x, trim = trim)))
unlist(lapply(trims, mean, x = x))

## -----------------------------------------------------------------------------
attach(mtcars)

formulas = list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)
rsquared <- function(mod) summary(mod)$r.squared

#loops
func_loop = vector("list", length(formulas))
for (i in seq_along(formulas)){
  func_loop[[i]] = lm(formulas[[i]], data = mtcars)
}
unlist(lapply(func_loop,rsquared))

# lapply
func_apply <- lapply(formulas, function(x) lm(formula = x, data = mtcars))
unlist(lapply(func_apply,rsquared))

## -----------------------------------------------------------------------------
x <- data.frame(
  seq = c(1:10),
  math = rnorm(10,80,3),
  Statistic = rnorm(10,90,1)
)
vapply(x, FUN = sd,FUN.VALUE = 0)

## -----------------------------------------------------------------------------
y <- data.frame(
  seq = c(1:3),
  name = c('张三','李四','王五'),
  math = rnorm(3,80,3),
  Statistic = rnorm(3,90,1)
)
vapply(y[vapply(y, FUN = is.numeric, FUN.VALUE = logical(1))], FUN = sd, FUN.VALUE = numeric(1))


## -----------------------------------------------------------------------------
mcsapply <- function(x, f, ...) {
  shuchu <- mclapply(f, ...)
  simplify2array(shuchu)
}

mcvapply <- function(x, f, FUN.VALUE, ...) {
  shuchu_list <- mclapply(f, ...)
  out <- matrix(rep(FUN.VALUE, length(x)), nrow = length(x))
  for (i in seq_along(x)) {
    stopifnot(
      length(res) == length(f.value),
      typeof(res) == typeof(f.value))
    out[i, ] <- shuchu_list[[i]]
  }
  out
}

## ----warning=FALSE------------------------------------------------------------
library(microbenchmark)

# 用C++写
library(Rcpp)
set.seed(5115)

sourceCpp(code='
#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
  NumericMatrix method_1(int n, int a, int b, int N){
    NumericMatrix shuchu(N, 2);
    double x1=1;
    double x2=0.66;
    double canshu1=1;
    double canshu2=1;
    for(int i=0; i < N; i++){
      x1 = rbinom(1, n, x2)[0];
      canshu1 = x1 + a;
      canshu2 = n - x1 + b;
      x2 = rbeta(1, canshu1, canshu2)[0];
      shuchu(i,0) = x1;
      shuchu(i,1) = x2;
    }
    return(shuchu);
  }
')

# 用R写
method_2 <- function(n, a, b, N){
  X <- matrix(0,N,2)
  X[1,] <- c(0,1)
  for (i in 2:N) {
    x2 <- X[i-1,2]
    X[i,1] <- rbinom(1, n, x2)
    x1 <- X[i,1]
    canshu1 <- x1+a
    canshu2 <- n-x1+b
    X[i,2] <- rbeta(1,canshu1,canshu2)
  }
  return(X)
}

#比较
N <- 5000
burn <- 1000
a <- 2
b <- 4
n <- 25
b <- burn+1

hanshu1 <- method_1(n,a,b,N)[b:N,]
hanshu2 <- method_2(n,a,b,N)[b:N,]

qqplot(hanshu1[,2], hanshu2[,2],xlab='r',ylab='c++')
abline(0, 1, lwd=2, col='red')

COMP = summary(microbenchmark(hanshu1,hanshu2))
knitr::kable(COMP)

