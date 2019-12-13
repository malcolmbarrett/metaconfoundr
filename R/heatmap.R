#' Title
#'
#' @param .df
#'
#' @return
#' @export
#'
#' @examples
mc_heatmap <- function(.df, legend_title = "control quality") {
  .df %>%
    ggplot2::ggplot(ggplot2::aes(x = variable, y = study, fill = control_quality)) +
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
