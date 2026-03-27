library(tidyverse)
library(haven)

cps <- read_sas("cps.sas7bdat")

data <- table(cps$union, cps$married) # table with desiered variables

result <- chisq.test(data) # chi squared test
print(result) # i wonder what this does? (It gives the result)