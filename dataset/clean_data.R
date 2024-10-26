library(tidyverse)

# Read in the datasets
data = read_csv("dataset/dataset-ignore/Most-Recent-Cohorts-Institution.csv")
data_cols = read_csv("dataset/cleaned_dataset.csv")

data_cols <- data_cols |> 
  filter(!is.na(Variable))

# Select the columns listed in data_cols$Variable
cleaned_data <- data |>
  select(all_of(data_cols$Variable))

write_rds(cleaned_data, file = here::here("dataset", "cleaned_dataset.rds"))

read_rds("dataset/cleaned_dataset.rds")