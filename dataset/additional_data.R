library(tidyverse)


GDP = read_csv("dataset/GDP.csv")
Population = read_csv("dataset/Population.csv")


write_rds(GDP, file = here::here("dataset", "GDP.rds"))
write_rds(Population, file = here::here("dataset", "Population.rds"))

read_rds("dataset/GDP.rds")
read_rds("dataset/Population.rds")

state_names <- c("Alaska", "Alabama", "Arkansas", "Arizona", "California", "Colorado", 
                 "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Iowa", 
                 "Idaho", "Illinois", "Indiana", "Kansas", "Kentucky", "Louisiana", 
                 "Massachusetts", "Maryland", "Maine", "Michigan", "Minnesota", 
                 "Missouri", "Mississippi", "Montana", "North Carolina", "North Dakota", 
                 "Nebraska", "New Hampshire", "New Jersey", "New Mexico", "Nevada", 
                 "New York", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
                 "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", 
                 "Virginia", "Vermont", "Wisconsin", "West Virginia", "Wyoming")
colnames(Population)[1:length(state_names) + 1] <- state_names

library(tidyverse)

# Step 1: Reshape Population to Long Format
Population_long <- Population %>%
  pivot_longer(
    cols = -observation_date, # All columns except observation_date
    names_to = "State",       # Name for the new column indicating state
    values_to = "Population"  # Name for the population values column
  ) %>%
  mutate(
    Year = as.numeric(substr(observation_date, 1, 4)),  # Extract year from observation_date
    State = gsub("POP$", "", State)                    # Remove "POP" suffix from state codes
  ) %>%
  select(State, Year, Population)                      # Keep relevant columns

# Step 2: Reshape GDP to Long Format
GDP_long <- GDP %>%
  pivot_longer(
    cols = as.character(1997:2023),  # Columns corresponding to years
    names_to = "Year",               # Name for the new column indicating year
    values_to = "GDP_Value"          # Name for the GDP values column
  ) %>%
  mutate(Year = as.numeric(Year))    # Convert Year to numeric

# Step 3: Merge the Datasets
combined_data <- GDP_long %>%
  left_join(Population_long, by = c("GeoName" = "State", "Year")) %>%
  mutate(GDP_per_Capita = GDP_Value / Population)  # Calculate GDP per capita
