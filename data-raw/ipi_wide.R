## code to prepare `ipi_wide` dataset goes here
library(tidyverse)
library(janitor)
ipi_wide <- readxl::read_xlsx("data-raw/ipi.xlsx") %>%
  clean_names() %>%
  mutate_at(vars(starts_with("study")), ~replace_na(., 0))

usethis::use_data(ipi_wide, overwrite = TRUE)
