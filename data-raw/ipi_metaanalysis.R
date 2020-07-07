## code to prepare `ipi_metaanalysis` dataset goes here
library(tidyverse)
library(janitor)
ipi_metaanalysis <- readxl::read_xlsx("data-raw/ipi2.xlsx", sheet = 2) %>%
  select(-`No.`, -`a,b`)

names(ipi_metaanalysis) <- c(
  "study",
  "lead_author",
  "year",
  "design",
  "effect_measure",
  "estimate",
  "lower_ci",
  "upper_ci",
  "sample_size"
)

usethis::use_data(ipi_metaanalysis, overwrite = TRUE)
