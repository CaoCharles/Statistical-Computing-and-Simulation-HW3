library(dplyr)
options(scipen = 100)
# Question 2
# define function
Q2 <- function(x){
 y <- (exp(x)-1)/(exp(1)-1)
 return(y)
}


# Monte-Carlo Integration -------------------------------------------------

theta.hat = NULL
for(i in 1:100){
x <- runif(10000)
theta.hat[i] <- mean(Q2(x))}
mean_actuall <- mean(theta.hat)
var_actuall <-var(theta.hat)

# Antithetic Variate ------------------------------------------------------
theta.hat = NULL
for(i in 1:100){
x <- runif(10000)
y <- 1-x
temp1 <- mean(Q2(x))
temp2 <- mean(Q2(y))
theta.hat[i] <- 0.5*(temp1+temp2)}
AV.mean <- mean(theta.hat)
AV.var <- var(theta.hat)
AV2.var <- 0.5*(var(temp1))(1 + cor(x,y))

# Importance Sampling -----------------------------------------------------

Q2 <- function(x){
  y <- (exp(x)-1)/(exp(1)-1)
  return(y)
}


theta.hat = NULL
for(i in 1:1000){
  x <- runif(1000)
  y <- runif(1000)
  temp1 <- mean(Q2(x))
  temp2 <- mean(Q2(y))
  theta.hat[i] <- 0.5*(temp1+temp2)}
Is.mean <- mean(theta.hat)
Is.var <- var(theta.hat)
