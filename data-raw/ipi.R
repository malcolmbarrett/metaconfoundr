## code to prepare `ipi` dataset goes here
library(tidyverse)
library(janitor)
ipi <- readxl::read_xlsx("data-raw/ipi.xlsx") %>%
  clean_names() %>%
  mutate_at(vars(starts_with("study")), ~replace_na(., 0)) %>%
  pivot_longer(cols = starts_with("study"), names_to = "study", values_to = "control_quality")

usethis::use_data(ipi, overwrite = TRUE)
