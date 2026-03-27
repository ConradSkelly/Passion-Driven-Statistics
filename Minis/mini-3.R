library(ggplot2)
# library(dplyr)
load("HELP.Rdata") # HELP

print_stats <- function(val, name) {
  cat(name)
  cat("\n")
  print(
    ggplot(data = HELP, mapping = aes(x = val)) + geom_bar()
  )
  print(
    ggplot(HELP, aes(x = val)) + geom_histogram(bins = 4)
  )
  print(transform(
    as.data.frame(table(val)),
    Percentage = Freq / sum(Freq) * 100
  ), row.names = FALSE)
  cat("\n")
}

extreme_mcs <- ifelse(HELP$mcs < 20, 1, 0)
suicidal_thought <- ifelse(HELP$g1b == "yes", 1, 0)
homeless_status <- ifelse(HELP$homeless == "homeless", 1, 0)
risk_total <- extreme_mcs + suicidal_thought + homeless_status
age <- HELP$age
print_stats(extreme_mcs, "ExtremeMCS")
print_stats(suicidal_thought, "SuicidalThought")
print_stats(homeless_status, "HomelessStatus")
print_stats(risk_total, "RiskTotal")
print_stats(age, "age")

