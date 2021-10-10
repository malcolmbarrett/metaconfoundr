set.seed(1234)

test_that("non-confounder functions work", {
  counts <- ipi %>%
    metaconfoundr() %>%
    count_non_confounders()

  expect_length(counts, 2)
  expect_named(counts, c("study", "n_non_confounders"))
  expect_equal(nrow(counts), dplyr::n_distinct(metaconfoundr(ipi)$study))

  p11 <- ipi %>%
    metaconfoundr() %>%
    plot_non_confounders()

  p12 <- ipi %>%
    metaconfoundr() %>%
    plot_non_confounders(size = 3, geom = ggplot2::geom_point)

  vdiffr::expect_doppelganger("Non confounder count: bar", p11)
  vdiffr::expect_doppelganger("Non confounder count: point", p12)
})
