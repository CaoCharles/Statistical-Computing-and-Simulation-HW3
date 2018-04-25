#5------------------------
#naive estimator--------

x=runif(100, min=0, max=2*pi)
epsilon=rnorm(100, mean=0, sd=0.09)
y=sin(x)+epsilon

NDE<-function(x,h=0.1){
  n=length(y)
  (y<(x+h)&y>(x-h)) %>% sum() %>% `/`(2*h*n)
}
range(y)

k<-c()
m<-seq(-1.133929,1.126161,0.05) #設定每一次要前進多少(x)
for(i in 1:length(m)){
  k[i]<-NDE(m[i],h = 0.2) #h
}

plot(m,k,type = "l")
lines(density(y))