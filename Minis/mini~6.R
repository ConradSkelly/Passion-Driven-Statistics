library(tidyverse)
library(haven)


data <- read_sas("cps.sas7bdat")

# from data filter where sex = male then get mean. Vice versa for Women
men_wage <- data %>%
  filter(sex == "M") %>%
  summarise(men_wage = mean(wage))

women_wage <- data %>%
  filter(sex == "F") %>%
  summarise(women_wage = mean(wage))

print(women_wage)
print(men_wage)

data.aov <- aov(wage ~ sex, data = data) # get the result of the ANOVA
data.cor <- cor.test(data$exper, data$wage, method = "pearson") # get the result of the pearson corrilation test
data.chi <- table(data$satisfaction, data$sector) # create a matrix composing of satification and sectors
data.chi.res <- chisq.test(data.chi) # get the result of the chi^2 test


# data visulization and displaying below.
summary(data.aov)
print(data.cor)
print(data.chi.res)


ggplot(data, aes(x = exper, y = wage)) +
  geom_point(color = "blue", size = 2) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Scatter Plot with Pearson Correlation",
       x = "experiance", y = "wage") +
  theme_minimal()