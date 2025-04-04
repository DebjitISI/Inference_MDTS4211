rpm = c(150,50,50,75,150,150,100,100,100,125,100,50,125,150,100,75,50,50,150,75)
level = c(5,1,1,2,5,5,3,3,3,4,3,1,4,5,3,2,1,1,5,2)

lpm = c(3.540,1.158,1.128,1.686,3.48,3.51,2.328,2.34,2.298,2.982,2.328,1.140,2.868,3.504,2.340,1.740,1.122,1.128,3.612,1.74)
obs =1:20

data = data.frame(rpm,level,lpm)
View(data)
attach(data)

avg_lpm = aggregate(lpm ~ level, data = data,FUN=mean)

y1_bar = mean(data[level == 1,'lpm'])
y2_bar = mean(data[level == 2,'lpm'])
y3_bar = mean(data[level == 3,'lpm'])
y4_bar = mean(data[level == 4,'lpm'])
y5_bar = mean(data[level == 5,'lpm'])
y_hat = c(y1_bar,y2_bar,y3_bar,y4_bar,y5_bar)

plot(1:5,y_hat)
plot(avg_lpm$level,avg_lpm$lpm,clab = 'Level',ylab ='Average lpm',main = 'Average lpm for different levels')

model = aov(lpm~as.factor(level),data=data)
r =resid(model)
plot(r)
abline(h=0)
mean(r)
shapiro.test(r)
qqnorm(r,pch=19,col='blue')
hist(r)
plot(r)
std_res=  rstandard(model)
std_res = std_res[which(abs(std_res)<2.5)]

 std_res = std_res[which(hatvalues(model)<(3*2/20))]
plot(std_res)

library(lmtest)
dwtest(model)
plot(model,1)


bartlett.test(lpm ~ as.factor(level))
plot(model,3)

## Bonferonni 
pairwise.t.test(lpm,as.factor(level),p.adjust.method = 'bonferroni')
#library(api2lm)
#confint_adjust(model,method = 'bonferroni')
library(DescTools)
PostHocTest(model,method = 'bonferroni')
ScheffeTest(model)
TukeyHSD(model)
?DunnettTest
DunnettTest(lpm ~ as.factor(level),data = data,control = '1')
