library(dplyr)
options(scipen = 100)
# Question 1
# X ~ Cauchy(0,1)
# 直接算機率
1 - pcauchy(1)
# Cauchy為中心點為0的對稱分佈
# P(X>1)=0.25、P(0<X<1)=0.25
# 接著我們使用下列方法來降低變異數
# Monte-Carlo Integration -------------------------------------------------

N <- 1000
cauchy <- function(x){
  return(1/(pi*(1+x^2)))
}
theta.hat = NULL
for(i in 1:1000){
  # 從cauchy抽取樣本
  x <- runif(N)
  x <- cauchy(x)
  theta.hat[i]<-mean(x)}
Mc.mean <- mean(theta.hat)
Mc.var <- var(theta.hat)
    
    

# Hit or miss -------------------------------------------------------------

N = 1000
theta.hat = NULL
for ( i in 1:1000){
x <- rcauchy(N)
N = 1000
theta.hat[i] <- mean(x > 1)}
Hm.mean <- mean(theta.hat)
Hm.var <- var(theta.hat)

# Antithetic Variate ------------------------------------------------------
# 對偶變量
N = 500
theta.hat = NULL
for( i in 1:1000){
x <- runif(N)
y <- 1 - x
temp1 <- cauchy(x)
temp2 <- cauchy(y)
theta.hat[i] <- 0.5 - 0.5*(mean(temp1) + mean(temp2))}
AV.mean <- mean(theta.hat)
AV.var <- var(theta.hat)

# Importance Sampling -----------------------------------------------------

N <- 1000
hes <- function(x){
  return(1/(pi*(1 + x^(-2))))
} 
theta.hat = NULL
for(i in 1:1000){
  U <- runif(N)
  x <- 1/U
  theta.hat[i] <- mean(hes(x))
}
Is.mean <- mean(theta.hat)
Is.var <- var(theta.hat)

# (3) ---------------------------------------------------------------------
N = 1000
theta.hat = NULL
cauchy <- function(x){
  return(1/(pi*(1+x^2)))}
for(i in 1:1000){
  x <- runif(N)
  theta.hat[i] <- (1- mean(2*cauchy(x)))/2
  }
three_mean <- mean(theta.hat)
three_var <- var(theta.hat)

# Control variate ---------------------------------------------------------
# ???

