# Load required packages and datasets
library(tidyverse)
library(scales)
patient_df <- read_rds("data/patients-tidy.rds")

# to check for any readmission
patient_df |>
  group_by(patient_id) |>
  summarize(
    count = n()
    ) |>
  filter(count > 1)
