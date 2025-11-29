# Load required packages and datasets
library(tidyverse)
library(dplyr)
library(scales)
patient_df <- read_rds("data/patients-tidy.rds")


patient_df |>
  group_by(service) |>
  summarize(frequency = n()) %>% arrange(desc(frequency))

# Total number of services rendered in the year
service_frequency_plot <- patient_df |>
  group_by(service) |>
  summarize(frequency = n()) |>
  ggplot(aes(x = frequency, y = reorder(service, frequency))) +
  geom_segment(aes(x = 0, xend = frequency, y = reorder(service, frequency), 
                   yend = reorder(service, frequency)),
               linewidth = 1, color = "grey70") +
  geom_point(size = 4, color = "steelblue") +
  theme_minimal() +
  labs(
    title = "Number of patients admitted in the year for each service",
    x = "Number of patients",
    y = "Service"
  )
ggsave("service-fequency-lollipop.png", service_frequency_plot, 
       height = 5, width = 8)


# Monthly admisssion trend by service 
monthly_admission_trend_service_plot <- patient_df |>
  group_by(service, month_of_admission) |>
  summarise(n = n()) |>
  ggplot(
    aes(x = month_of_admission, y = n, color = service, group = service)
  ) +
  geom_point() +
  geom_line()+
  labs(
    title = "Trend in monthly admissions by service",
    y     = "Number of patients admitted",
    x     = "Month",
    color = "Service"
  )
ggsave(
  "monthly-admission-trend-service.png",
  monthly_admission_trend_service_plot,
  height = 5, width = 15
  )


# I am keeping this for keepsake. I was trying to create the monthly admission 
# trend by service plot and got stuck, all the points were joined together, so 
# i decided to create each plot manually but i knew there must be a better way 
# until chatgpt helped me out, the problem was in the group for the line chat, i 
# was supposed to set the group as the variable and not 1. i was stuck for the
# rest of the day and overnight, i kept going over the code for errors!
#(just playing around by highlighting the data points for surgery)
patient_df |>
  group_by(service, month_of_admission) |>
  summarise(n = n()) |>
  pivot_wider(
    id_cols = month_of_admission,
    names_from = service,
    values_from = n
  ) |>
  ggplot(
    aes(x = month_of_admission)
  ) +
  geom_point(aes(y = ICU), color = "darkred", size = 2) +
  geom_line(aes(y = ICU), group = 1, linewidth = 0.5, 
            color = "steelblue", alpha = 0.5) +
  geom_point(aes(y = emergency), color = "green", size = 2) +
  geom_line(aes(y = emergency), group = 1, linewidth = 0.5,
            color = "purple", alpha = 0.5) +
  geom_point(aes(y = general_medicine), color = "pink", size = 2) +
  geom_line(aes(y = general_medicine), group = 1, linewidth = 0.5, 
            color = "black", alpha = 0.5) +
  geom_point(aes(y = surgery), color = "orange", size = 2) +
  geom_line(aes(y = surgery), group = 1, linewidth = 1, color = "forestgreen") +
  labs(
    title = "Trend in monthly admissions by service",
    y     = "Number of patients admitted",
    x     = "Month",
  )


# weekly admisssion trend by service for only general medicine and surgery
weekly_admission_trend_service_plot <- patient_df |>
  filter(service %in% c("general_medicine", "surgery")) |>
  group_by(service, week_of_admission) |>
  summarise(n = n()) |>
  ggplot(
    aes(x = week_of_admission, y = n, color = service, group = service)
  ) +
  geom_point() +
  geom_line()+
  scale_x_continuous(breaks = seq(2, 52, by = 2))+
  scale_y_continuous(breaks = seq(2, 12, by = 2))+
  labs(
    title = "Trend in weekly admissions by service",
    y     = "Number of patients admitted",
    x     = "Week",
    color = "Service"
  )
ggsave(
  "weekly-admission-trend-service.png",
  weekly_admission_trend_service_plot,
  height = 5, width = 20
)


#Hospital service by age
Service_by_age_plot <- patient_df |>
  ggplot(
    aes(
      x = service,
      fill = age_group, height = 10, width = 20
    )
  ) +
  geom_bar(position = "fill", width = 0.8) +
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   color = "forestgreen", size = 4)) +
  theme(axis.text.y = element_text(color = "forestgreen", size = 4)) +
  theme(axis.title.x = element_text(color = "purple", size = 6)) +
  theme(axis.title.y = element_text(color = "purple", size = 6)) +
  theme(axis.title = element_text(color = "purple", size = 6)) +
  theme(legend.text = element_text(size = 4)) +
  theme(legend.title = element_text(size = 6)) +
  theme(legend.key.size = unit(0.6, "lines")) +
  theme(title = element_text(size = 8, color = "purple")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(
    title    = "The age distribution of patients admitted 
    for each service",
    caption  = "Data source : kaggle",
    x = "Service", y = "Proportion (%)",
    fill = "Age group"
  )
ggsave("service-by-age.png", Service_by_age_plot)
