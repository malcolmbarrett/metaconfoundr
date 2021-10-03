## code to prepare `ipi` dataset goes here
library(tidyverse)
library(janitor)
ipi <- read_csv("data-raw/ipi_updated_2021-09.csv") %>%
  select(-1) %>%
  mutate_at(-1:-3, ~replace_na(., 0)) %>%
  clean_names()

usethis::use_data(ipi, overwrite = TRUE)
