library(tidyverse)
library(haven)


data <- read_sas("utilities.sas7bdat")

monthly_gas_average <- data %>%
  group_by(month) %>%
  summarise(mean_gas_bill = mean(gasbill))

monthly_electric_average <- data %>%
  group_by(month) %>%
  summarise(mean_electric_bill = mean(elecbill))

data <- data %>% mutate(seasons = case_when(
  month %in% c(12,1,2) ~ "Winter",
  month %in% c(3,4,5) ~ "Spring",
  month %in% c(6,7,8) ~ "Summer",
  month %in% c(9,10,11) ~ "Fall"
))

donate_ <- data %>%
  group_by(seasons) %>%
  summarise(donate_precentage = mean(ifelse(donate == "yes", 1, 0))) %>%
    arrange()


table(monthly_gas_average)
table(monthly_electric_average)

print(ggplot(data, aes(x = totalbill)) + geom_histogram(binwidth = 50))
print(ggplot(monthly_gas_average, aes(x = as.factor(month), y = mean_gas_bill)) + geom_col())
print(ggplot(monthly_electric_average, aes(x = as.factor(month), y = mean_electric_bill)) + geom_col())
print(ggplot(data, mapping = aes(x = kwh, y = gasbill)) + geom_point())
cor.test(data$kwh, data$gasbill, method = "pearson")
print(ggplot(data, mapping = aes(x = kwh, y = elecbill)) + geom_point())
cor.test(data$kwh, data$elecbill, method = "pearson")

# this below is my magnum opus
print(ggplot(
  arrange(donate_, donate_precentage),
  aes(
    x = fct_inorder(as.character(seasons)),
    y = donate_precentage)) +
        geom_col()
)




