library(ggplot2)
set.seed(106354012)

x <- rbeta(100, shape1 = 2, shape2 = 3, ncp = 0)
hist(x)
x <- as.data.frame(x)
ggplot(x, aes(x)) + 
  geom_histogram(aes(y = ..count..), bins = 9)  


