test_that("scoring and sorting works", {
  filtered_ipi <- ipi %>%
    metaconfoundr() %>%
    dplyr::filter(is_confounder == "Y")

  x1 <- score_control(filtered_ipi)
  x2 <- score_control(filtered_ipi, "sum")
  x3 <- score_control(filtered_ipi, "controlled")

  expect_type(x1$score, "double")
  expect_type(x2$score, "double")
  expect_type(x3$score, "double")
})
