#' Title
#'
#' @param .df
#'
#' @return
#' @export
#'
#' @examples
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
    ggplot2::geom_tile(color = "white", size = .8)
}

#' Title
#'
#' @param .df
#'
#' @return
#' @export
#'
#' @examples
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
      dplyr::arrange(desc(score)) %>%
      dplyr::mutate(variable = fct_inorder(variable)) %>%
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

#' Title
#'
#' @param ... Arguments passed to the underline scale function
#'
#' @return
#' @export
#'
#' @examples
scale_fill_cochrane <- function(...) {
  ggplot2::scale_fill_manual(
    values = c("#D85247FF", "#E2B01EFF", "#409C58FF"),
    ...
  )
}


#' @export
#' @rdname scale_fill_cochrane
scale_color_cochrane <- function(...) {
  ggplot2::scale_color_manual(
    values = c("#D85247FF", "#E2B01EFF", "#409C58FF"),
    ...
  )
}

#' @export
#' @rdname scale_fill_cochrane
scale_shape_cochrane <- function(...) {
  ggplot2::scale_shape_manual(values = c(120, 45, 43))
}

#' Title
#'
#' @param base_size
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom ggplot2 `%+replace%`
theme_mc <- function(base_size = 14) {
  ggplot2::theme_minimal(base_size = base_size) %+replace%
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}
