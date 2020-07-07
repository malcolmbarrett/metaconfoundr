## code to prepare `ipi` dataset goes here
library(tidyverse)
library(janitor)
ipi <- readxl::read_xlsx("data-raw/ipi2.xlsx") %>%
  mutate_at(-1:-3, ~replace_na(., 0)) %>%
  mutate_at(-1:-3, ~ . - 1) %>%
  pivot_longer(cols = -1:-3, names_to = "study", values_to = "control_quality") %>%
  clean_names()

usethis::use_data(ipi, overwrite = TRUE)
