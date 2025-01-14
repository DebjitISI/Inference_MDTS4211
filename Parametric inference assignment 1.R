#1
n=10;R=100;lambda= 2; pi= 0.1

zip= function(lambda,pi){
  lambda_hat = pi_hat=pi_hat_mod= array(dim= 1)
  
  for(k in 1:R){
    y= array(dim = 1)
    for(i in 1:n){
      u= runif(1,0,1)
      y[i]= ifelse(u<pi,0,rpois(1,lambda))
    }
    s_sq= var(y)*(n-1)/n
    y_bar= mean(y)
    lambda_hat[k]= s_sq/y_bar+ y_bar-1
    pi_hat[k]= (lambda_hat[k]-y_bar)/lambda_hat[k]
    pi_hat_mod[k] = max(0,pi_hat)
  }
  par(mfrow= c(1,3))
  plot(lambda_hat, pch= 10, main= "MME of lambda")
  abline(h= lambda)
  plot(pi_hat, pch= 10, main= "MME of pi")
  abline(h= pi)
  plot(pi_hat_mod, pch= 10, main= "Modified MME of pi")
  abline(h= pi)
}

zip(2,0.1)


#3
rm(list=ls())
#(a)
n=5
mu=0
sigma=1
x=rcauchy(n,mu,sigma)
lik=function(mu){
  L=prod(dcauchy(x,mu,1))
  return(L)
}
#(b)
L=array(dim=1)
mu=seq(-4,4,0.01)
for(i in 1:length(mu)){
  L[i]=lik(mu[i])
}
par(mfrow=c(1,1))
plot(mu,L,type="l")

#(c)
mu_mle=mu[L==max(L)]
abline(v=mu_mle)

#(d)
#install.packages("nleqslv")
library(nleqslv)
?nleqslv
lk=function(mu){
  v=array(dim=1)
  for(i in 1:n){
    v[i]=(x[i]-mu)/(1+(x[i]-mu)**2)
  }
  sum(v)
}

nleqslv(median(x),lk,method = "Newton")$x
mu_mle

#4 Find the bivariate log likelihood function--outer(), parsp()
rm(list=ls())
#(a)
n=5
mu=0
sigma=1
x=rcauchy(n,mu,sigma)
lik=function(mu,sigma){
  L=prod(dcauchy(x,mu,sigma))
  return(L)
}
#(b)
L=array(dim=1)
mu=seq(-4,4,0.01)
sigma=seq(0.1,1,0.1)
for(j in 1:length(sigma)){
  for(i in 1:length(mu)){
    L[i]=lik(mu[i],sigma[j])
  }
}
par(mfrow=c(1,1))
persp(x=seq(-4,4,0.01),y=seq(0.1,1,0.1),L)

#(c)
mu_mle=mu[L==max(L)]
abline(v=mu_mle)
