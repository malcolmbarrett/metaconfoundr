expect_mc_sum_tbl <- function(x) {
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_named(x, c("construct", "variable", "study", "control_quality"), ignore.order = TRUE)
  expect_length(x, 4)
  expect_true(is.ordered(x$control_quality))
  expect_true(all(levels(x$control_quality) %in% c("inadequate", "some concerns", "adequate")))
}

test_that("summarize_control_quality() works", {
  x1 <- summarize_control_quality(
    metaconfoundr(ipi),
    Sociodemographics = `Maternal age` & `Race/ethnicity` & `Marital status`,
    Socioeconomics = `SES category` | Insurance & Education,
    "Reproductive Hx" = `Prior pregnancy outcome`
  )

  expect_mc_sum_tbl(x1)

  expect_error(
    summarize_control_quality(
      metaconfoundr(ipi),
      Sociodemographics = `Doesn't exist` & `Race/Ethnicity` & `Marital Status`,
      Socioeconomics = `SES Category` | Insurance & Education,
      "Reproductive Hx" = `Prior Pregnancy Outcome`
    )
  )

  x2 <- summarize_control_quality(
    metaconfoundr(ipi),
    "Sociodemographics = `Maternal age` & `Race/ethnicity` & `Marital status`,
    Socioeconomics = `SES category` | Insurance & Education,
    \"Reproductive Hx\" = `Prior pregnancy outcome`"
  )

  expect_mc_sum_tbl(x2)

  x3 <- summarize_control_quality(
    metaconfoundr(ipi),
    `Maternal age` & `Race/ethnicity` & `Marital status`,
    `SES category` | Insurance & Education,
    `Prior pregnancy outcome`
  )

  expect_mc_sum_tbl(x3)

  expect_true(all(unique(x3$variable) %in% c("overall", "domain1", "domain2", "domain3")))

  x4 <- summarize_control_quality(
    metaconfoundr(ipi),
    `Maternal age` & `Race/ethnicity` & `Marital status`,
    `SES category` | Insurance & Education,
    `Prior pregnancy outcome`,
    domains = FALSE
  )

  expect_mc_sum_tbl(x4)

  expect_true(unique(x4$variable) == "overall")
})
