#' Title
#'
#' @param .df
#'
#' @return
#' @export
#'
#' @examples
mc_heatmap <- function(.df, legend_title = "control quality", sort = FALSE, by_group = FALSE, score = c("sum", "adequate", "controlled")) {

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
    ggplot2::geom_tile(color = "white", size = .8) +
    ggplot2::labs(fill = legend_title)
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
