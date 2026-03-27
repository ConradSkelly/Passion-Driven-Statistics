library(tidyverse)
library(haven)

utilities <- read_sas("utilities.sas7bdat")

utilities <- utilities %>%
  mutate(season = case_when(
    month %in% c(12, 1, 2) ~ "Winter",
    month %in% c(3, 4, 5)  ~ "Spring",
    month %in% c(6, 7, 8)  ~ "Summer",
    month %in% c(9, 10, 11) ~ "Fall"
  ))

data <- aov(totalbill ~ as.factor(season), data = utilities)
summary(data)

data <- TukeyHSD(data)
data <- as.data.frame(data$`as.factor(season)`)

data <- data[data$'p adj' < 0.05,]

print(data)