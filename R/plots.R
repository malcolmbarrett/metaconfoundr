#' Plot a heatmap or traffic light plot of `metaconfoundr()` summaries
#'
#' `mc_heatmap()` and `mc_trafficlight()` visualize the results of
#' `metaconfoundr()`, summarizing the quality of confounder control in each
#' study.
#'
#' @param .df A data frame, usually the result of `metaconfoundr()`
#' @param legend_title The legend title
#' @param sort Logical. Sort by confounder score? Calculated by [score_control()]
#' @param size The size of the points in the traffic light plot
#' @param by_group Logical. If sorted, sort within domain?
#' @param non_confounders Logical. Include non-confounders? Default is `FALSE`.
#' @inheritParams score_control
#'
#' @return a ggplot
#' @export
#'
#' @examples
#'
#' ipi %>%
#'   metaconfoundr() %>%
#'   dplyr::mutate(variable = stringr::str_wrap(variable, 10)) %>%
#'   mc_heatmap() +
#'   theme_mc() +
#'   facet_constructs() +
#'   ggplot2::guides(x = ggplot2::guide_axis(n.dodge = 2))
#'
#' ipi %>%
#'   metaconfoundr() %>%
#'   mc_trafficlight() +
#'   geom_cochrane() +
#'   facet_constructs() +
#'   scale_fill_cochrane() +
#'   theme_mc() +
#'   ggplot2::guides(x = ggplot2::guide_axis(n.dodge = 2))
#'
#' @family plots
mc_heatmap <- function(.df, legend_title = "control quality", sort = FALSE, by_group = FALSE, score = c("adequate", "sum", "controlled"), non_confounders = FALSE) {

  .df <- filter_confounders(.df, non_confounders)

  p <- process_plot(
    .df,
    legend_title = legend_title,
    sort = sort,
    by_group = by_group,
    score = score,
    non_confounders = non_confounders
  )

  p +
    ggplot2::geom_tile(color = "white", linewidth = .8)
}


#' @export
#' @rdname mc_heatmap
mc_trafficlight <- function(.df, size = 8, legend_title = "control quality", sort = FALSE, by_group = FALSE, score = c("adequate", "sum", "controlled"), non_confounders = FALSE) {

  .df <- filter_confounders(.df, non_confounders)

  p <- process_plot(
    .df,
    legend_title = legend_title,
    sort = sort,
    by_group = by_group,
    score = score,
    non_confounders = non_confounders
  )

  p +
    ggplot2::geom_point(color = "white", size = size, shape = 21)
}

filter_confounders <- function(.df, non_confounders) {
  if (!non_confounders & "is_confounder" %in% names(.df)) {
    .df <- dplyr::filter(.df, is_confounder %in% c("Y", "Yes", "TRUE", "1"))
  }

  .df
}

process_plot <- function(.df, legend_title = "control quality", sort = FALSE, by_group = FALSE, score = c("adequate", "sum", "controlled"), non_confounders = FALSE) {
  if (!sort)  p <- ggplot2::ggplot(.df, ggplot2::aes(x = variable, y = study, fill = control_quality))

  if (sort && !by_group) {
    p <- .df %>%
      score_control(score = score) %>%
      dplyr::arrange(dplyr::desc(score)) %>%
      dplyr::mutate(variable = forcats::fct_inorder(variable)) %>%
      ggplot2::ggplot(ggplot2::aes(x = variable, y = study, fill = control_quality))
  }

  if (sort && by_group) {
    p <- .df %>%
      reorder_variable() %>%
      ggplot2::ggplot(ggplot2::aes(x = variable, y = study, fill = control_quality)) +
      scale_x_reordered()
  }

  p +
    ggplot2::labs(
      fill = legend_title,
      shape = legend_title
    ) +
    scale_shape_cochrane()
}

GeomCochrane <- ggplot2::ggproto("GeomCochrane", ggplot2::GeomPoint,
  default_aes = ggplot2::aes(
    shape = 43,
    colour = "white",
    size = 5,
    fill = NA,
    alpha = NA,
    stroke = 0.5
  )
)

#' Add Cochrane-style symbols to heatmaps and traffic light plots
#'
#'
#' @inheritParams ggplot2::geom_point
#'
#' @return a geom
#' @export
#'
#' @family plots
geom_cochrane <- function(mapping = ggplot2::aes(shape = control_quality), data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCochrane,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' Add Cochrane-style palettes to ggplots
#'
#' @param ... Arguments passed to the underline scale function
#'
#' @return scales for ggplot
#' @export
#'
#' @family plots
scale_fill_cochrane <- function(...) {
  ggplot2::scale_fill_manual(
    values = c(
      "inadequate" = "#D85247FF",
      "some concerns" = "#E2B01EFF",
      "adequate" = "#409C58FF"
    ),
    ...
  )
}


#' @export
#' @rdname scale_fill_cochrane
scale_color_cochrane <- function(...) {
  ggplot2::scale_color_manual(
    values = c(
      "inadequate" = "#D85247FF",
      "some concerns" = "#E2B01EFF",
      "adequate" = "#409C58FF"
    ),
    ...
  )
}

#' @export
#' @rdname scale_fill_cochrane
scale_shape_cochrane <- function(...) {
  ggplot2::scale_shape_manual(
    values = c(
      "inadequate" = 120,
      "some concerns" = 45,
      "adequate" = 43
    ),
    ...
  )
}

#' A minimal theme for metaconfoundr plots
#'
#' @inheritParams ggplot2::theme_minimal
#'
#' @return a ggplot theme
#' @export
#'
#' @importFrom ggplot2 `%+replace%`
#' @family plots
theme_mc <- function(base_size = 14) {
  ggplot2::theme_minimal(base_size = base_size) %+replace%
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}

# from tidytext, for internal use
scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

#' Facet by constructs
#'
#' A helper function to facet by constructs in `[mc_heatmap()] and
#' [mc_trafficlight()]
#'
#' @param ... Arguments passed to [`ggplot2::facet_grid()`]
#'
#' @return a facet component
#' @export
#'
#' @family plots
facet_constructs <- function(...) {
  ggplot2::facet_grid(. ~ construct, scales = "free_x", space = "free_x", ...)
}

#' Label values using ROBINS approach
#'
#' `label_robins()` is a helper function to modify metaconfoundr labels to use
#' ROBINS-like labels: low risk, some concerns, high risk.
#'
#' @return a character vector of ROBINS labels
#' @export
#'
#' @examples
#' mc_heatmap(metaconfoundr(ipi)) +
#'   ggplot2::scale_fill_ordinal(labels = label_robins())
#'
#' mc_heatmap(metaconfoundr(ipi)) +
#'   scale_fill_cochrane(labels = label_robins())
#'
label_robins <- function() {
  c(
    "adequate" = "low risk",
    "some concerns" = "some concerns",
    "inadequate" = "high risk"
  )
}

