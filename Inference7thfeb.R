X= c(9.29,10.15,8.69,11.25,6.58,9.76,12.05,12.38,7.88,11.56,10.25,8.93,9.02,10.87,10.0)

library(DescTools)
SignTest(X,alternative = "greater",mu=9.9,conf.level = 0.95)

shapiro.test(X)

qqplot(c(1:length(X)),y=X)

hist(X,freq=F)
lines(density(X))

t.test(X,mu=9.9)
#install.packages('lawstat')
library(lawstat)
symmetry.test(X)




R=100
mu=seq( from=0.01,to=1.00,by=.1)
mu
emp_power=array()
vect=array()
for (j in 1:length(mu)){
  set.seed(123)
  for (i in 1:R){
    X=rnorm(20,mu[j])
    t=mean(X)*sqrt(20)/sd(X)
    c=qnorm(.95,mean=mu)                            
    vect[i]=ifelse(t>c,1,0)
  }
  emp_power[j]=sum(vect)/R
}

emp_power

n=seq(from=20, to=100,by=10)
mu=0.5
emp_power=array()
vect=array()
for (j in 1:length(n)){
  set.seed(123)
  for (i in 1:R){
    X=rnorm(n[i],mu)
    t=mean(X)*sqrt(20)/sd(X)
    c=qnorm(.95,mean=mu)                            
    vect[i]=ifelse(t>c,1,0)
  }
  emp_power[j]=sum(vect)/R
}

emp_power