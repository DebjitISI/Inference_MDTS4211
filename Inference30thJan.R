?t.test
X=c(43,46,57,45,55,26,234,200,212,253,260,251)
t.test(X,alternative = "greater",mu=140)
       
?binom.test
binom.test(sum(X > 140), length(data), p=0.5, alternative="greater")
library(DescTools)
SignTest(X,alternative = "greater",mu=140,conf.level = 0.95)

?shapiro.test
shapiro.test(X)

qqplot(c(1:length(X)),y=X)

hist(X,freq=F)
lines(density(X))
