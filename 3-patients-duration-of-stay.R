# Load required packages and datasets
library(tidyverse)
library(scales)
patient_df <- read_rds("data/patients-tidy.rds")


# Plot duration of stay in the year
mean(patient_df$duration_of_stay, na.rm = TRUE)
median(patient_df$duration_of_stay, na.rm = TRUE)
max(patient_df$duration_of_stay, na.rm = TRUE)
min(patient_df$duration_of_stay, na.rm = TRUE)

duration_of_stay_plot <- ggplot(patient_df, aes(x = duration_of_stay)) +
  geom_histogram()
ggsave("duration-of-stay-plot.png", duration_of_stay_plot)


# Relationship between duration of stay and age/age group
duration_of_stay_age_plot <- ggplot(patient_df,
       aes(x = age_group,
           y = duration_of_stay)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(color = "forestgreen")) +
  labs(
    title = "Differences in length of admission (in days) among various 
    age groups",
    x = "Age group",
    y = "length of hospital stay (days)"
  )

# Relationship between duration of stay and unit
duration_of_stay_service_plot <- ggplot(patient_df,
       aes(x = service,
           y = duration_of_stay)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(color = "forestgreen")) +
  labs(
    title = "Differences in length of admission (in days) among hospital 
    departments",
    x = "hospital department",
    y = "length of hospital stay (days)"
  )

ggsave("duration-of-stay-service-plot.png", duration_of_stay_service_plot,  
       height = 10, width = 10)
ggsave("duration-of-stay-age-plot.png", duration_of_stay_age_plot, height = 10,
       width = 10)


