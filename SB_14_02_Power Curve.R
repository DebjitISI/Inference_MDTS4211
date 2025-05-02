rm(list = ls())
par(mfrow = c(1,2))

##power curve
mu0 = 0
mu1 = seq(0.1,1.5,0.1)
n = 20
R = 1000

emperical.power = numeric(length(mu1))
emperical.power1 = numeric(length(mu1))
emperical.power.wsr = numeric(length(mu1))

for(j in 1:length(mu1)){
  set.seed(123)
  vec = numeric(R)
  vec1 = numeric(R)
  vec.wsr = numeric(R)
  for(i in 1:R){
    data = rnorm(n, mean = mu1[j], sd = 1)
    
    #t-test
    x.bar = mean(data)
    s1 = sqrt(1/(n-1)*sum((data - x.bar)^2))
    test.stat1 = (x.bar - mu0)/(s1/sqrt(n))
    
    #sign test
    k = ifelse(data>mu0, 1, 0); 
    s2 = sum(k)
    test.stat2 = (s2 - (n/2))/(sqrt(n/4))
  
    #wilcoxon sign rank test
    w.wsr = sum(k*rank(abs(data-mu0)))
    mean.wsr = (n*(n+1))/4
    var.wsr = (n*(n+1)*(2*n+1))/24
    test.stat.wsr = (w.wsr - mean.wsr)/sqrt(var.wsr)
    
    vec[i] = ifelse(test.stat1 > qt(0.95, n-1), 1, 0)
    vec1[i] = ifelse(test.stat2 > qnorm(0.95), 1, 0)
    vec.wsr[i] = ifelse(test.stat.wsr > qnorm(0.95), 1 ,0)
  }
  emperical.power[j] = sum(vec)/R
  emperical.power1[j] = sum(vec1)/R
  emperical.power.wsr[j] = sum(vec.wsr)/R
}

emperical.power
emperical.power1
emperical.power.wsr

plot(mu1, emperical.power, type = "l", main = "Power Curve", col = "blue")
lines(mu1, emperical.power1, col = "green")
lines(mu1, emperical.power.wsr, col = "red")


legend(0.1,1, legend = c("t-test", "sign test", "WSR test"), fill = c("blue", "green", "red"))


##Consistency of the test
mu0 = 0
mu1 = 0.5
n = seq(10,100,10)
R = 1000

power = numeric(length(n))
power1 = numeric(length(n))
power.wsr = numeric(length(n))

for(j in 1:length(n)){
  set.seed(123)
  vec2 = numeric(R)
  vec3 = numeric(R)
  vec.wsr1 = numeric(R)
  for(i in 1:R){
    #t test
    data = rnorm(n[j], mean = mu1, sd = 1)
    x.bar = mean(data)
    s3 = sqrt(1/(n[j]-1)*sum((data - x.bar)^2))
    test.stat3 = (x.bar - mu0)/(s3/sqrt(n[j]))
    
    #sign test
    k = ifelse(data>mu0, 1, 0); 
    s4 = sum(k)
    test.stat4 = (s4 - (n[j]/2))/(sqrt(n[j]/4))
    
    #wsr test
    w.wsr = sum(k*rank(abs(data-mu0)))
    mean.wsr = (n[j]*(n[j]+1))/4
    var.wsr = (n[j]*(n[j]+1)*(2*n[j]+1))/24
    test.stat.wsr1 = (w.wsr - mean.wsr)/sqrt(var.wsr)
    
    vec2[i] = ifelse(test.stat3 > qt(0.95,n[j]-1), 1, 0)
    vec3[i] = ifelse(test.stat4 > qnorm(0.95), 1, 0)
    vec.wsr1[i] = ifelse(test.stat.wsr1 > qnorm(0.95), 1 ,0)
  }
  power[j] = sum(vec2)/R
  power1[j] = sum(vec3)/R
  power.wsr[j] = sum(vec.wsr1)/R
}
power

plot(n,power, type = "l", main = "Consistency of the test", col = "blue")
lines(n, power1, col = "green")
lines(n, power.wsr, col = "red")

legend(80,0.6, legend = c("t-test", "sign test", "WSR test"), fill = c("blue", "green", "red"))
