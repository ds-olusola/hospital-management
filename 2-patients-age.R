# Load required packages and dataset
library(tidyverse)
library(scales)
patient_df <- read_rds("data/patients-tidy.rds")


# Total number of hospital visits by week and month
weekly_admission_plot <- patient_df |>
  group_by(week_of_admission) |>
  summarise(n = n()) |>
  ggplot(
    aes(x = week_of_admission, y = n)
  ) +
  geom_point(color = "darkred", size = 2) +
  geom_line(linewidth = 1, color = "steelblue") +
  labs(
    title = "Trend in weekly Admissions",
    y     = "Number of patients admitted",
    x     = "Week"
  )


monthly_admission_plot <- patient_df |>
  group_by(month_of_admission) |>
  summarise(n = n()) |>
  ggplot(
    aes(x = month_of_admission, y = n)
  ) +
  geom_point(color = "darkred", size = 2) +
  geom_line(group = 1, linewidth = 1, color = "steelblue") +
  labs(
    title = "Trend in Monthly Admissions",
    y     = "Number of patients admitted",
    x     = "Month"
  )


ggsave("weekly-admission-trend.png", plot = weekly_admission_plot)
ggsave("monthly-admmission-trend.png", plot = monthly_admission_plot)


# Age distribution of the patients
patient_age_plot <- patient_df |>
  ggplot(
    aes(x = age_group)
  ) +
  geom_bar(fill = "orange") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, 
                                   color = "forestgreen")) +
  theme(axis.text.y = element_text(color = "forestgreen")) +
  theme(axis.title.x = element_text(color = "purple")) +
  theme(axis.title.y = element_text(color = "purple")) +
  theme(axis.title = element_text(color = "purple")) +
  labs(
    title    = "Patients' age distribution",
    caption  = "Source : kaggle",
    x = "Age group (years)", y = "Frequency"
  )


# patient age distribution by service
patient_age_by_unit_plot <- patient_df |>
  ggplot(
    aes(
      x = age_group,
      fill = service
    )
  ) +
  geom_bar(position = "fill", width = 0.8) +
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   color = "forestgreen", size = 6)) +
  theme(axis.text.y = element_text(color = "forestgreen", size = 6)) +
  theme(axis.title.x = element_text(color = "purple", size = 8)) +
  theme(axis.title.y = element_text(color = "purple", size = 8)) +
  theme(axis.title = element_text(color = "purple", size = 8)) +
  theme(legend.text = element_text(size = 8)) +
  theme(legend.title = element_text(size = 7)) +
  theme(legend.key.size = unit(0.6, "lines")) +
  theme(title = element_text(size = 10, color = "purple")) +
  labs(
    title    = "Patients' age distribution by unit admitting",
    caption  = "Source : kaggle",
    x = "Age group (years)", y = "Proportion (%)",
    fill = "Admitting Unit"
  )

ggsave("patient-age-barplot.png", patient_age_plot)
ggsave("patient-age-by-unit-barplot.png", patient_age_by_unit_plot,
       width = 5, height = 5)

