# Create a dataframe in R
df <- data.frame(
  Observations = 1:16,
  Duty = c("regular", "regular", "regular", "heavy", "regular", "regular", "regular", "heavy",
           "heavy", "regular", "regular", "heavy", "heavy", "heavy", "heavy", "heavy"),
  Brand = c("name", "store", "name", "store", "name", "name", "store", "name",
            "store", "store", "store", "name", "store", "name", "store", "name"),
  Life_per_unit_cost = c(611, 923, 537, 476, 542, 593, 794, 445, 
                         569, 827, 898, 490, 480, 384, 460, 413)
)

# Print dataframe
print(df)
attach(df)

# Interaction plot to check interaction effect
interaction.plot(x.factor = Brand, 
                 trace.factor = Duty, 
                 response = Life_per_unit_cost, 
                 fun = mean, 
                 col = c("red", "blue"), 
                 lty = 1, 
                 lwd = 2, 
                 xlab = "Brand", 
                 ylab = "Life per unit cost", 
                 main = "Interaction Plot: Duty vs Brand")




anova_model <- aov(Life_per_unit_cost ~ Duty*Brand , data = df)


library(car)
library(lmtest)


mean=array()
a=0
for(i in unique(Duty)){
  for( j in unique(Brand)){
    a=a+1
    mean[a]=mean(df[(Duty==i & Brand == j),'Life_per_unit_cost'])
  }
}

plot(mean,type ='l')

library(dplyr)
plot(r)
abline(h=0)

qqplot(x=index(r) ,y=r)


#Normality Checking
r = resid(anova_model)
shapiro.test(r)

#Heteroscedasticity
plot(anova_model,1)
abline(h=0,v=0)
plot(anova_model,3)
abline(h=0,v=0)
bptest(anova_model)

bartlett.test(Life_per_unit_cost ~ Brand)
bartlett.test(Life_per_unit_cost ~ Duty)

#Multicollinearity
hat_values = hatvalues(model)

summary(anova_model)

library(car)
VIF(model)

library(DescTools)
PostHocTest(anova_model,method = 'bonferroni')
ScheffeTest(anova_model)
TukeyHSD(anova_model)
?DunnettTest
DunnettTest(Life_per_unit_cost ~ as.factor(Duty)*as.factor(Brand),data = df,control = '1')

anova_model$coefficients
anova_model$