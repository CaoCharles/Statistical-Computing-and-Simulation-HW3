library(dplyr)
options(scipen = 100)
# Question 2
# define function
Q2 <- function(x){
 y <- (exp(x)-1)/(exp(1)-1)
 return(y)}
N = 1000
# Monte-Carlo Integration -------------------------------------------------

theta.hat = NULL
<<<<<<< HEAD
for(i in 1:N){
x <- runif(N)
=======
for(i in 1:100){
x <- runif(10000)
>>>>>>> c66219c34e51a361f9ed756f0bb4e9d5b6236d13
theta.hat[i] <- mean(Q2(x))}
Mc.mean <- mean(theta.hat)
Mc.var <-var(theta.hat)

# Antithetic Variate ------------------------------------------------------

theta.hat = NULL
<<<<<<< HEAD
for(i in 1:N){
x <- runif(N/2)
y <- 1 - x
=======
for(i in 1:100){
x <- runif(10000)
y <- 1-x
>>>>>>> c66219c34e51a361f9ed756f0bb4e9d5b6236d13
temp1 <- mean(Q2(x))
temp2 <- mean(Q2(y))
theta.hat[i] <- 0.5*(temp1+temp2)}
AV.mean <- mean(theta.hat)
AV.var <- var(theta.hat)
<<<<<<< HEAD
=======
AV2.var <- 0.5*(var(temp1))(1 + cor(x,y))
>>>>>>> c66219c34e51a361f9ed756f0bb4e9d5b6236d13

# Importance Sampling -----------------------------------------------------

theta.hat1 = NULL
theta.hat2 = NULL
theta.hat3 = NULL
theta.hat4 = NULL
for( i in 1:N){
  
  u <- runif(N)     #f3, inverse transform method
  x <- - log(1 - u * (1 - exp(-1)))
  fg <- Q2(x) / (exp(-x) / (1 - exp(-1)))
  theta.hat1[i] <- mean(fg)
  
  u <- runif(N/2)     #f3, inverse transform method + Antithetic Variate
  v <- 1 - u
  x1 <- - log(1 - u * (1 - exp(-1)))
  x2 <- - log(1 - v * (1 - exp(-1)))
  fg1 <- Q2(x1) / (exp(-x1) / (1 - exp(-1)))
  fg2 <- Q2(x2) / (exp(-x2) / (1 - exp(-1)))
  fg <- 0.5*(fg1+fg2)
  theta.hat2[i] <- mean(fg)

  u <- runif(N)    #f4, inverse transform method
  x <- tan(pi * u / 4)
  fg <- Q2(x) / (4 / ((1 + x^2) * pi))
  theta.hat3[i] <- mean(fg)

  u <- runif(N/2)    #f4, inverse transform method + Antithetic Variate
  v <- 1 - u
  x1 <- tan(pi * u / 4)
  x2 <- tan(pi * v / 4)
  fg1 <- Q2(x1) / (4 / ((1 + x1^2) * pi))
  fg2 <- Q2(x2) / (4 / ((1 + x2^2) * pi))
  fg <- 0.5*(fg1+fg2)
  theta.hat4[i] <- mean(fg)
}

mean11 <- c(mean(theta.hat1 ),mean(theta.hat2 ),mean(theta.hat3 ),mean(theta.hat4 ))
var11 <- c(var(theta.hat1 ),var(theta.hat2 ),var(theta.hat3 ),var(theta.hat4))
mean11
var11
# Control variate ---------------------------------------------------------

f <- function(x){
  exp(x)-1}
theta.hat = NULL
for( i in 1:N){
  u <- runif(10000)
  B <- f(u)
  A <- Q2(u)
  a <- -cov(A,B) / var(B)
  u <- runif(N)
  T1 <- Q2(u)
  T2 <- T1 + a * (f(u) - exp(1)+2)
  theta.hat <- mean(T2)
  }
mean(theta.hat)
var(theta.hat)

# Stratified Sampling -----------------------------------------------------


