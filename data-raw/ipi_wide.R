## code to prepare `ipi_wide` dataset goes here
library(tidyverse)
library(janitor)
ipi_wide <- readxl::read_xlsx("data-raw/ipi2.xlsx") %>%
  clean_names() %>%
  mutate_at(-1:-3, ~replace_na(., 0)) %>%
  mutate_at(-1:-3, ~ . - 1)

usethis::use_data(ipi_wide, overwrite = TRUE)
