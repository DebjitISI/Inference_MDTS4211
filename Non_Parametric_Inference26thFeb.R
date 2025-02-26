#library(VGAM)
#power of performance (t-test)
med=log(2,base = exp(1))
power_func1=function(n,mu){
  set.seed(123)
  R=1000
  tobs=power=array(0)
  for(i in 1:R){
    sample=rexp(n,1/mu)
    tobs[i]=(mean(sample)-med)/(sd(sample)/sqrt(n))
  }
  result=ifelse(tobs>qt(0.95,n-1),1,0)
  power=sum(result)/R
  return(power)
}
#power of performance(sign test)
#rl
power_func2=function(n,mu){
  set.seed(123)
  R=1000
  p=1/2
  q=1/2
  tobs=power=array(0)
  for(i in 1:R){
    sample=rexp(n,1/mu)
    S=sum(ifelse(sample>med,1,0))
    tobs[i]=(S-n*q)/sqrt(n*p*q)
    
  }
  result=ifelse(tobs>qexp(0.95),1,0)
  power=sum(result)/R
  return(power)
  
}

#power of performance(Wilcoxon test)
power_func3=function(n,mu){
  set.seed(123)
  R=1000
  tobs=power=array(0)
  for(i in 1:R){
    sample=rexp(n,1/mu)
    S1=ifelse(sample>med,1,0)
    w=sum(S1*(rank(abs(sample-med))))
    tobs[i]=(w-n*(n+1)/4)/sqrt(n*(n+1)+(2*n+1)/24)
    
  }
  result=ifelse(tobs>qexp(0.95),1,0)
  power=sum(result)/R
  return(power)
  
}
# for varying alternatives
par(mfrow=c(1,2))
mu=seq(0.1,2,0.01);n=20;Power1=Power2=Power3=array(0)
for(i in 1:length(mu)){
  Power1[i]=power_func1(n,mu[i])
  Power2[i]=power_func2(n,mu[i])
  Power3[i]=power_func3(n,mu[i])
}
plot(mu,Power1,type='l',
     xlab='Alternative',ylab='Power',
     main='Power of Performance')
lines(mu,Power2,col="Red")
lines(mu,Power3,col="Green")
legend(0.1,1,c("t-test","Sign test","Wilcoxon test"),fill=c("black","red","Green"))

# for varying sample size
n=seq(10,110,5);mu=1.5;Power1=Power2=Power3=array(0)
for(i in 1:length(n)){
  Power1[i]=power_func1(n[i],mu)
  Power2[i]=power_func2(n[i],mu)
  Power3[i]=power_func3(n[i],mu)
}
plot(n,Power1,type='l',
     xlab='Sample Size',ylab='Power',
     main='Power of Performance')
lines(n,Power2,col="Red")
lines(n,Power3,col="Green")
legend(60,.6,c("t-test","Sign test","Wilcoxon test"),fill=c("black","red","Green"))

