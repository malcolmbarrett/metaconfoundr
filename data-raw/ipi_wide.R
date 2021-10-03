## code to prepare `ipi_wide` dataset goes here
library(tidyverse)
library(janitor)
ipi_wide <- read_csv("data-raw/ipi_updated_2021-09.csv") %>%
  select(-1) %>%
  mutate_at(-1:-3, ~replace_na(., 0)) %>%
  clean_names() %>%
  pivot_wider(names_from = study, values_from = control_quality)

usethis::use_data(ipi_wide, overwrite = TRUE)
