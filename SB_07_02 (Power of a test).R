##Power Curve
rm(list = ls())
mu0 = 0
mu1 = seq(0.5,1.5,0.1)
n = 20
R = 1000
emperical.power = numeric(length(mu1))

for(j in 1:length(mu1)){
  set.seed(123)
  vec = numeric(R)
  for(i in 1:R){
    data = rnorm(n, mean = mu1[j], sd = 1)
    x.bar = mean(data)
    s = sqrt(1/(n-1)*sum((data - x.bar)^2))
    test.stat = (x.bar - mu0)/(s/sqrt(n))
    
    vec[i] = ifelse(test.stat > qt(0.95, n-1), 1, 0)
  }
  emperical.power[j] = sum(vec)/R
}

emperical.power

plot(mu1, emperical.power, type = "l", main = "Power Curve")

##Consistency of the test
mu0 = 0
mu1 = 0.5
n = seq(10,100,1)
R = 1000

power = numeric(length(n))

for(j in 1:length(n)){
  set.seed(123)
  vec1 = numeric(R)
  for(i in 1:R){
    data = rnorm(n[j], mean = mu1, sd = 1)
    x.bar = mean(data)
    s = sqrt(1/(n[j]-1)*sum((data - x.bar)^2))
    test.stat = (x.bar - mu0)/(s/sqrt(n[j]))
    
    vec1[i] = ifelse(test.stat > qt(0.95,n[j]-1), 1, 0)
  }
  power[j] = sum(vec1)/R
}
power

plot(n,power, type = "l", main = "Consistency of the test")

?qt
