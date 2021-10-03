# saveRDS(
#   readLines(file.path("inst", "WORDLIST")),
#   file.path(".aspell", "metaconfoundr.Rds")
# )

Rd_files <- vignettes <- R_files <- description <-
  list(
    encoding = "UTF-8",
    language = "en",
    dictionaries = c("en_stats", "metaconfoundr")
  )
