library(tidyverse)
library(haven)

data <- read_sas("cps.sas7bdat")

# pearson corrilation moderating for sex.
by(data, data$sex, function(x)
     cor.test(x$wage, x$exper))

# normal anova.
myAnovaResults <- aov(wage ~
sector, data = data)
summary(myAnovaResults)

# anova moderating for race.
by(data, data$race, function(x)
list(aov(wage ~ sector, data = x), summary(aov( wage ~ sector, data = x))))
