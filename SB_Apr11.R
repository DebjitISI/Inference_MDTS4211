# Creating the data frame from Table 14-8

breaking_strength <- data.frame(
  Machine = rep(c("Machine1", "Machine2", "Machine3"), each = 5),
  y = c(36, 41, 39, 42, 49,     # Machine 1
        40, 48, 39, 45, 44,     # Machine 2
        35, 37, 42, 34, 32),    # Machine 3
  x = c(20, 25, 24, 25, 32,     # Machine 1
        22, 28, 22, 30, 28,     # Machine 2
        21, 23, 26, 21, 15)     # Machine 3
)

# View the data frame
print(breaking_strength)
attach(breaking_strength)

ancova_model = aov(y ~ x + as.factor(Machine), data = breaking_strength)

r = resid(ancova_model)
shapiro.test(r)

plot(x = breaking_strength$x,y=breaking_strength$y,main = 'Relationship b/w x and y')

summary(ancova_model)
plot(ancova_model,1)
plot(ancova_model,3)
plot(r)
abline(h=0,v=0)

#autocorrelation
durbinWatsonTest(ancova_model)

#Heteroscedasticity
library(lmtest)
bptest(ancova_model)
bartlett.test(y ~ as.factor(Machine),data = breaking_strength) # only possible for 1 factor variable

hat_values = hatvalues(ancova_model)

#summary(ancova_model)

library(car)
VIF(ancova_model)
aov(breaking_strength$x ~ as.factor(Machine))

ancova_model$coefficients
summary(ancova_model)
