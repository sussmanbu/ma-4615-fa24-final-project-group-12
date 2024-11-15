library(tidyverse)


GDP = read_csv("dataset/GDP.csv")
Population = read_csv("dataset/Population.csv")


write_rds(GDP, file = here::here("dataset", "GDP.rds"))
write_rds(Population, file = here::here("dataset", "Population.rds"))

read_rds("dataset/GDP.rds")
read_rds("dataset/Population.rds")