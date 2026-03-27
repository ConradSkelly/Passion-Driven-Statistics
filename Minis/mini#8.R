library(tidyverse)
library(haven)
library(ppcor)

data <- read_sas("saratogahouses.sas7bdat")


data.cor.bath <- cor.test(data$Baths, data$Price, method = "pearson")
model.bath <- lm(Price ~ Baths, data = data)
coefficients.bath <- coef(model.bath)
intercept.bath <- coefficients.bath[1]
slope.bath <- coefficients.bath[2]

data.cor.area <- cor.test(data$Living_Area, data$Price, method = "pearson")
model.area <- lm(Price ~ Living_Area, data = data)
coefficients.area <- coef(model.area)
intercept.area <- coefficients.area[1]
slope.area <- coefficients.area[2]
#
# pcor.test(data$QuantResponseVar,
#           data$QuantExplanatoryVar,
#           data$Living_Area)

print(data.cor.bath)
print(intercept.bath)
print(slope.bath)
summary(model.bath)

print(data.cor.area)
print(intercept.area)
print(slope.area)
summary(model.area)

plot(data$Baths, data$Price)
abline(model.bath, col="red")

# Student_health <- read_sas("studenthealth.sas7bdat")
#
# mean_weight <- Student_health %>%
#   group_by(major) %>%
#   summarise(weight = mean(weight))
#
# print(mean_weight)
#
# weight.aov.WM <- aov(weight ~ major, data = Student_health)
# weight.aov.WG <- aov(weight ~ gender, data = Student_health)
#
#
# summary(weight.aov.WM)
# summary(weight.aov.GM)