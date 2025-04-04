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
summary(anova_model)

shapiro.test(resid(anova_model))

plot(anova_model,1)
plot(anova_model,3)

library(car)
library(lmtest)

bptest(anova_model)

bartlett.test(Life_per_unit_cost ~ Brand)
bartlett.test(Life_per_unit_cost ~ Duty)
