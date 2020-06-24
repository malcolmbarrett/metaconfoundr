expect_mc_tbl <- function(x) {
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_named(x, c("construct", "variable", "is_confounder", "study", "control_quality"), ignore.order = TRUE)
  expect_length(x, 5)
  expect_true(is.ordered(x$control_quality))
  expect_true(all(levels(x$control_quality) %in% c("inadequate", "some concerns", "adequate")))
}


test_that("metaconfoundr works", {
  x1 <- metaconfoundr(ipi)
  x2 <- metaconfoundr(ipi_wide)
  expect_mc_tbl(x1)
  expect_mc_tbl(x2)
})

test_that("layout detection works", {
  expect_error(metaconfoundr(ipi, mc_wider()))
  expect_error(metaconfoundr(ipi_wide, mc_longer()))

  ipi_wide2 <- ipi_wide %>%
    dplyr::rename(scope = construct)

  expect_error(metaconfoundr(ipi_wide2))

  expect_mc_tbl(metaconfoundr(ipi_wide2, mc_wider(construct = "scope")))
})
