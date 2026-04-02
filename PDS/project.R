library(tidyverse)
library(chisq.posthoc.test)
library(rstatix)

# the data frame from the csv is assigned to the data variable
data <- read.csv("brfss2021.csv")

# a custom funtion to get make a frequency table
frequency_table <- function(data, column) {
  data %>%
  filter(!is.na({{ column }})) %>%
  group_by( {{ column }} ) %>%
  summarise(Frequency = n()) %>%
  mutate(Percentage = (Frequency / sum(Frequency)) * 100)
}

# cull unwanted response variables and then change dummy code to be better
did_exercise <- data %>%
  filter(!EXERANY2 %in% c(7, 9)) %>%
  mutate(EXERANY2 = ifelse(EXERANY2 == 2, 0, EXERANY2)) %>%
  frequency_table(EXERANY2)

employment_status <- data %>%
  filter(!EMPLOY1 %in% c(9)) %>%
  frequency_table(EMPLOY1)

# cull unwanted responces variables change dummy code to be better and then make a bin
bad_mental_health_days <- data %>%
  filter(!MENTHLTH %in% c(77, 99)) %>%
  mutate(
    MENTHLTH = ifelse(MENTHLTH == 88, 0, MENTHLTH),
    MENTHLTH = cut(MENTHLTH,
      breaks = c(-1, 0, 7, 14, 21, 30),
      labels = c("None", "1-7 days", "8-14 days", "15-21 days", "22-30 days")
    )
  ) %>%
  frequency_table(MENTHLTH)

debilitating_mental_health_days <- data %>%
  filter(!POORHLTH %in% c(77, 99)) %>%
  mutate(
    POORHLTH = ifelse(POORHLTH == 88, 0, POORHLTH),
    POORHLTH = cut(POORHLTH,
      breaks = c(-1, 0, 7, 14, 21, 30),
      labels = c("None", "1-7 days", "8-14 days", "15-21 days", "22-30 days")
    )
  ) %>%
  frequency_table(POORHLTH)


exercise_employment <- data %>%
  filter(!EXERANY2 %in% c(7, 9),
         !EMPLOY1 %in% c(9)) %>%
  mutate(EXERANY2 = ifelse(EXERANY2 == 2, 0, EXERANY2))

# bivariate anlysis
contingency_table <- table(exercise_employment$EMPLOY1, exercise_employment$EXERANY2)
chi_result <- chisq.test(contingency_table)
chi_post_hoc_result <- chisq.posthoc.test(contingency_table, method = "bonferroni")
cramer_v <- cramer_v(contingency_table)

# dsiplay data :)
print(did_exercise)
print(employment_status)
print(bad_mental_health_days)
print(debilitating_mental_health_days)
print(chi_result)
print(cramer_v)
print(chi_post_hoc_result)

# Did Exercise
ggplot(did_exercise, aes(x = factor(EXERANY2, labels = c("No", "Yes")), y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.5) +
  labs(
    title = "Exercise in Past 30 Days",
    x = "Did Exercise",
    y = "Percentage (%)"
  ) +
  theme_minimal()

# Employment Status
ggplot(employment_status, aes(x = factor(EMPLOY1, labels = c("Employed for wages", "Self-employed", "not-working > 1 year", "not-working < 1 year", "Homemaker", "Student", "Retired",  "Unable to work")), y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.5) +
  labs(
    title = "Employment Status",
    x = "Employment Category",
    y = "Percentage (%)"
  ) +
  theme_minimal()

# Bad Mental Health Days
ggplot(bad_mental_health_days, aes(x = MENTHLTH, y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.5) +
  labs(
    title = "Poor Mental Health Days in Past 30 Days",
    x = "Number of Days",
    y = "Percentage (%)"
  ) +
  theme_minimal()

# Debilitating Mental Health Days
ggplot(debilitating_mental_health_days, aes(x = POORHLTH, y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.5) +
  labs(
    title = "Days Poor Health Impacted Daily Activities in Past 30 Days",
    x = "Number of Days",
    y = "Percentage (%)"
  ) +
  theme_minimal()


exercise_employment %>%
  filter(!is.na(EMPLOY1), !is.na(EXERANY2)) %>%
  group_by(EMPLOY1, EXERANY2) %>%
  summarise(num = n()) %>%
  group_by(EMPLOY1) %>%
  mutate(Percentage = num / sum(num) * 100) %>%
  filter(EXERANY2 == 1) %>%
  ggplot(aes(x = factor(EMPLOY1, labels = c("Employed for wages", "Self-employed",
             "not-working > 1 year", "not-working < 1 year",
             "Homemaker", "Student", "Retired", "Unable to work")),
             y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.5) +
  labs(title = "% Who Exercised in Past 30 Days by Employment Status",
       x = "Employment Category", y = "% Who Exercised") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

residuals_df <- as.data.frame(chi_result$stdres) %>%
  rename(EMPLOY1 = Var1, EXERANY2 = Var2, Residual = Freq) %>%
  filter(EXERANY2 == 1)