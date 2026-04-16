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

sex_dist <- model_data %>%
  filter(!EXERANY2 %in% c(7, 9)) %>%
  frequency_table(BIRTHSEX)

race_dist <- model_data %>%
  filter(!EMPLOY1 %in% c(9)) %>%
  frequency_table(X._RACEGR3)

edu_dist <- model_data %>%
  filter(!EMPLOY1 %in% c(9)) %>%
  frequency_table(X._EDUCAG)

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

# Sex
ggplot(sex_dist, aes(x = factor(BIRTHSEX, labels = c("Male", "Female")), y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.5) +
  labs(
    title = "Sex Distribution",
    x = "Sex",
    y = "Percentage (%)"
  ) +
  theme_minimal()

# Race/Ethnicity
ggplot(race_dist, aes(x = factor(X._RACEGR3, labels = c("White Non-Hispanic", "Black Non-Hispanic",
                                                          "Other Non-Hispanic", "Multiracial Non-Hispanic",
                                                          "Hispanic")), y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.5) +
  labs(
    title = "Race / Ethnicity Distribution",
    x = "Race / Ethnicity",
    y = "Percentage (%)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Education
ggplot(edu_dist, aes(x = factor(X._EDUCAG, labels = c("Did not graduate HS", "Graduated HS",
                                                        "Attended College/Technical",
                                                        "Graduated College/Technical")), y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.5) +
  labs(
    title = "Education Distribution",
    x = "Education Level",
    y = "Percentage (%)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

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

# <-------------moderating variables------------->

# clean moderating vars
model_data <- data %>%
  filter(
    !EXERANY2    %in% c(7, 9),
    !EMPLOY1     %in% c(9),
    !BIRTHSEX    %in% c(7, 9),
    !X._RACEGR3  %in% c(9),
    !X._EDUCAG   %in% c(9)
  ) %>%
  mutate(EXERANY2 = ifelse(EXERANY2 == 2, 0, EXERANY2))

# custom function to run chi-square + cramer's v within each stratum
stratified_chisq <- function(data, moderator_var, moderator_labels) {
  levels <- sort(unique(data[[moderator_var]]))
  levels <- levels[!is.na(levels)]

  for (i in seq_along(levels)) {
    cat("\n──", moderator_labels[i], "──\n")
    subset_data <- data %>% filter(.data[[moderator_var]] == levels[i])
    ct  <- table(subset_data$EMPLOY1, subset_data$EXERANY2)
    chi <- chisq.test(ct)
    cv  <- cramer_v(ct)
    post_hoc <- chisq.posthoc.test(ct, method = "bonferroni")
    print(chi)
    cat("Cramer's V:", cv, "\n")
    cat("Post-hoc results:\n")
    print(post_hoc)
  }
}

# stratified by biological sex
cat("\n═══ Stratified by Sex ═══")
stratified_chisq(
  model_data, "BIRTHSEX",
  c("Male", "Female")
)

# stratified by race / ethnicity
cat("\n═══ Stratified by Race/Ethnicity ═══")
stratified_chisq(model_data, "X._RACEGR3", c("White Non-Hispanic", "Black Non-Hispanic",
                  "Other Non-Hispanic", "Multiracial Non-Hispanic", "Hispanic"))

# stratified by education
cat("\n═══ Stratified by Education ═══")
stratified_chisq(model_data, "X._EDUCAG", c("Did not graduate HS", "Graduated HS",
                  "Attended College/Technical", "Graduated College/Technical"))

employ_labels <- c("Employed\nfor wages", "Self-\nemployed", "Not working\n>1 year",
                   "Not working\n<1 year", "Homemaker", "Student", "Retired", "Unable\nto work")

# helper to build the plot data
exercise_by_group <- function(data, moderator_var, moderator_labels) {
  levels <- sort(unique(data[[moderator_var]]))
  levels <- levels[!is.na(levels)]

  data %>%
    filter(!is.na(.data[[moderator_var]]), !is.na(EMPLOY1), !is.na(EXERANY2)) %>%
    mutate(moderator_label = factor(
      .data[[moderator_var]],
      levels = levels,
      labels = moderator_labels
    )) %>%
    group_by(moderator_label, EMPLOY1, EXERANY2) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(moderator_label, EMPLOY1) %>%
    mutate(Percentage = n / sum(n) * 100) %>%
    filter(EXERANY2 == 1)
}

# biological sex plot
exercise_by_group(model_data, "BIRTHSEX", c("Male", "Female")) %>%
  ggplot(aes(x = factor(EMPLOY1, labels = employ_labels), y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.5, size = 3) +
  facet_wrap(~moderator_label) +
  labs(title = "% Who Exercised by Employment Status, Stratified by Sex",
       x = "Employment Category", y = "% Who Exercised") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# race / ethnicity plot
exercise_by_group(model_data, "X._RACEGR3",
                  c("White Non-Hispanic", "Black Non-Hispanic", "Other Non-Hispanic",
                    "Multiracial Non-Hispanic", "Hispanic")) %>%
  ggplot(aes(x = factor(EMPLOY1, labels = employ_labels), y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.5, size = 3) +
  facet_wrap(~moderator_label) +
  labs(title = "% Who Exercised by Employment Status, Stratified by Race/Ethnicity",
       x = "Employment Category", y = "% Who Exercised") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# education plot
exercise_by_group(model_data, "X._EDUCAG",
                  c("Did not graduate HS", "Graduated HS",
                    "Attended College", "Graduated College")) %>%
  ggplot(aes(x = factor(EMPLOY1, labels = employ_labels), y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.5, size = 3) +
  facet_wrap(~moderator_label) +
  labs(title = "% Who Exercised by Employment Status, Stratified by Education",
       x = "Employment Category", y = "% Who Exercised") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))