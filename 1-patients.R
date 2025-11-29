library(tidyverse)
library(dplyr)
library(lubridate)
library(scales)

patients_df <- read_csv("data/patients.csv")
patients_df


# create a new variable with age categories
patient_df <- patients_df |>
  mutate(
    age_group = case_when(
      age < 5  ~ "under_five",
      age < 15 ~ "older_children",
      age < 25 ~ "young_adults",
      age < 45 ~ "older_adults",
      age < 65 ~ "middle_age",
      age >= 65 ~ "elderly"
    ),
    .after = age
  )

# covert age_group to an ordered factor
patient_df <- patient_df |>
  mutate(
    age_group = factor(
      age_group,
      levels = c("under_five", "older_children", "young_adults", 
                 "older_adults", "middle_age", "elderly"),
      ordered = TRUE
      )
  )


# create duration_of_stay variable
patient_df <- patient_df |>
  mutate(
    duration_of_stay = departure_date - arrival_date,
    .after = departure_date
  )


# create week and month variables
patient_df <- patient_df|>
  mutate(
    week_of_admission  = isoweek(arrival_date),
    month_of_admission = factor(
      month(arrival_date),
      levels = 1:12,
      labels = month.abb,
      ordered = TRUE
      ),
    .before = duration_of_stay
  )

# save tidy data into a new file for future use
write_rds(patient_df, "data/patients-tidy.rds")
