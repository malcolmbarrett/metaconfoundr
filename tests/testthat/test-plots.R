set.seed(1234)

test_that("heatmap works", {
  mc_ipi <- ipi %>%
    metaconfoundr()

  mc_ipi_fct <- mc_ipi %>%
    dplyr::mutate(variable = forcats::fct_inorder(variable))

  p1 <- mc_heatmap(mc_ipi_fct)

  p2 <- mc_heatmap(mc_ipi, sort = TRUE)

  p3 <- mc_heatmap(mc_ipi, sort = TRUE, by_group = TRUE) +
    facet_constructs()

  p4 <- mc_heatmap(mc_ipi_fct) +
    geom_cochrane() +
    scale_fill_cochrane() +
    scale_color_cochrane()

  vdiffr::expect_doppelganger("Basic heatmap", p1)
  vdiffr::expect_doppelganger("Themed heatmap", p1 + theme_mc())
  vdiffr::expect_doppelganger("Sorted heatmap", p2)
  vdiffr::expect_doppelganger("Sorted heatmap by domain", p3)
  vdiffr::expect_doppelganger("Cochrane heatmap", p4)
})

test_that("traffic light plot works", {
  mc_ipi <- ipi %>%
    metaconfoundr()

  mc_ipi_fct <- mc_ipi %>%
    dplyr::mutate(variable = forcats::fct_inorder(variable))

  p5 <- mc_trafficlight(mc_ipi_fct)

  p6 <- mc_trafficlight(mc_ipi, sort = TRUE)

  p7 <- mc_trafficlight(mc_ipi, sort = TRUE, by_group = TRUE) +
    facet_constructs()

  p8 <- mc_trafficlight(mc_ipi_fct) +
    geom_cochrane() +
    scale_fill_cochrane() +
    scale_color_cochrane()

  vdiffr::expect_doppelganger("Basic traffic light plot", p5)
  vdiffr::expect_doppelganger("Themed traffic light plot", p5 + theme_mc())
  vdiffr::expect_doppelganger("Sorted traffic light plot", p6)
  vdiffr::expect_doppelganger("Sorted traffic light plot by domain", p7)
  vdiffr::expect_doppelganger("Cochrane traffic light plot", p8)
})

test_that("`label_robins()` correctly modifies labels", {
  p9 <- mc_heatmap(metaconfoundr(ipi)) +
    ggplot2::scale_fill_ordinal(labels = label_robins())

  p10 <- mc_heatmap(metaconfoundr(ipi)) +
    scale_fill_cochrane(labels = label_robins())

  vdiffr::expect_doppelganger("Heatmap with ROBINS labels", p9)
  vdiffr::expect_doppelganger("Heatmap with ROBINS labels, Cochrane colors", p10)

})
