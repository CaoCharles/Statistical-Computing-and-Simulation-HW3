library(ggplot2)
# 1.	First, simulate 100 observations from a mixed distribution of beta(2,3)
# , each with probability 0.5. Then, use at least 3 density estimating methods
# to smooth the observations. 
# You need to specify the parameters in the smoothing methods, 
# and compare the results. 

set.seed(106354012)

x <- rbeta(100, shape1 = 2, shape2 = 3, ncp = 0)
hist(x)
x <- as.data.frame(x)
ggplot(x, aes(x)) + 
  geom_histogram(aes(y = ..count..), bins = 9)  


