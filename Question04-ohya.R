
# First, simulate 100 observations from a mixed distribution of beta(2,3)
# , each with probability 0.5. Then, use at least 3 density estimating methods
# to smooth the observations. 
# You need to specify the parameters in the smoothing methods, 
# and compare the results.

set.seed(20)
m = 100
x = rbeta(m,2,3)
# 畫個
stripchart(x,pch=16,cex=0.5,col=3,main="Dotplot")
y = seq(0,1,length=100)
hist(x,breaks = 5,probability = T,ylim = c(0,3))
lines(y,dbeta(y,2,3),col=4)
legend("right","Beta(2,3)",lty=1,col="Blue")

kernal_g = density(x, kernel = c("gaussian"),width = 0.1)
kernal_r = density(x, kernel = c("rectangular"),width = 0.1)
kernal_t = density(x, kernel = c("triangular"),width = 0.1)

lines(kernal_g)
lines(kernal_r)
lines(kernal_t)

# 寫code~~~
set.seed(0123)
x <- rbeta(100,2,3)
h=0.1
#histogram density estimator
histest=function(x,h){
  w=function(x,a,b){
    if (x<=b & x>=a) {return(1)}
    else {return(0)}}
  n=length(x)
  sx=seq(min(x),max(x),by=h)
  a=sx[-length(sx)]
  b=sx[-1]
  ni=NULL
  for (j in 1:length(a)){
    ni[j]=sum(x<=b[j] & x>=a[j])}
  t1=NULL
  for (i in sort(x)){
    t0=NULL
    for (j in 1:length(a)){
      wi=w(i,a[j],b[j])
      t0=c(t0,wi)}
    y=1/n*sum(ni/h*t0)
    t1=c(t1,y)}
  return(t1)}

# naive density estimator
naiveest=function(x,h){
  w=function(y){
    if (abs(y)<1) {return(1/2)}
    else {return(0)}}
  n=length(x)
  sx=seq(min(x),max(x),length=100)
  t1=NULL
  for (i in sx){
    t0=NULL
    for (j in x){
      wei=w((i-j)/h)
      t0=c(t0,wei)}
    y=1/n*sum(1/h*t0)
    t1=c(t1,y)}
  return(t1)}

# kernel density estimator
# norm
kernel_norm=function(x,h){
  w=function(y){ dnorm(y) }
  n=length(x)
  sx=seq(min(x),max(x),length=100)
  t1=NULL
  for (i in sx){
    t0=NULL
    for (j in x){
      wei=w((i-j)/h)
      t0=c(t0,wei)}
    y=1/n*sum(1/h*t0)
    t1=c(t1,y)}
  return(t1)}

y1=histest(x,0.1)                #h=0.1，也可使用其他h值
xa=seq(min(x),max(x),length=500)
y2=naiveest(x,0.1)               #h=0.1，也可使用其他h值
y3=kernel_norm(x,0.1)            #h=0.1，也可使用其他h值

par(mfrow=c(1,1))
plot(sort(x),y1,typ="l",xlab="x",ylab="f(x)",xlim=c(0,1), ylim=c(0,3))

j <- seq(0,1,length=500)
yy <- dbeta(j,2,3)
plot(yy)
lines(seq(0,1,length=5000),dbeta(j,2,3),col = "skyblue")
matplot(cbind(xa,xa),cbind(y2,y3),typ=c("l","l"),
        pch=1,col=c(2,4),lty=c(2:3),main=" Three Density Estimates ",add=TRUE)
legend(1,0.3,c("histogram(h=0.1)","naive(h=0.1)","kernel(h=0.1)")
       ,col=c(1,2,4),lty=c(1:3),cex=0.8)
xx <- sort(x)
yy <- dbeta(xx,2,3)
y2 <- naiveest(xx,0.1)
y3 <- kernel_norm(xx,0.1)
data <- cbind(xx,yy,y2,y3) %>% as.data.frame()

colnames(data) <- c("sample","Beta(2,3)","naive","kernal")
LBJ <- gather(data,key = "type",value = "value",2:4)
LBJ$type %<>% as.factor()
library(tidyverse)
ggplot(data = data,aes(x=sample))+
  geom_histogram(mapping = aes(y=..density..),fill="gray")+
  geom_line(mapping = aes(y=hist),color="#FFFF00",size=1)+
  geom_line(mapping = aes(y=naive),color="#FF00FF",size=1,linetype=1.5)+
  geom_line(mapping = aes(y=kernal),color="#00FFFF",size=1,linetype=1.5)+ 
  scale_x_continuous(breaks=seq(0,1,0.1))+
  theme_set(theme_bw())+
  theme(panel.grid =element_blank())+
  theme(panel.grid.major=element_line(colour=NA))+
  theme(legend.position = c(1,1))

ggplot(data = LBJ)+
  geom_histogram(mapping = aes(x=sample,y=..density..),color="white")+
  geom_line(mapping = aes(x=sample,y=value,color=type,group=type),size=1.2)+
  theme(legend.title = element_text(colour="royalblue", size=20, face="bold"),legend.text = element_text(size = 16, face = "bold"),legend.position = c(0.8,0.8))+
  theme(panel.grid =element_blank())
