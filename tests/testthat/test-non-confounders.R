context("non-confounder-plots")

test_that("non-confounder functions work", {
  counts <- ipi %>%
    metaconfoundr() %>%
    count_non_confounders()

  expect_length(counts, 2)
  expect_named(counts, c("study", "n_non_confounders"))
  expect_equal(nrow(counts), dplyr::n_distinct(metaconfoundr(ipi)$study))

  p1 <- ipi %>%
    metaconfoundr() %>%
    plot_non_confounders()

  p2 <- ipi %>%
    metaconfoundr() %>%
    plot_non_confounders(size = 3, geom = ggplot2::geom_point)

  vdiffr::expect_doppelganger("Non confounder count: bar", p1)
  vdiffr::expect_doppelganger("Non confounder count: point", p2)
})
