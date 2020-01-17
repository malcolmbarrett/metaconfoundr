#' Title
#'
#' @param .df
#' @param score
#'
#' @return
#' @export
#'
#' @examples
score_control <- function(.df, score = c("sum", "adequate", "controlled")) {
  score <- match.arg(score)
  f <- get_score_f(score)

  .df %>%
    dplyr::group_by(variable, add = TRUE) %>%
    dplyr::mutate(score = f(as.numeric(control_quality))) %>%
    dplyr::ungroup()
}


get_score_f <- function(score) {
  if (score == "sum") return(sum)
  if (score == "adequate") return(mean_adequate)

  mean_controlled
}

mean_adequate <- function(x) mean(x == 3, na.rm = TRUE)

mean_controlled <- function(x) mean(x != 1, na.rm = TRUE)

reorder_variable <- function(.df, score = c("sum", "adequate", "controlled"), ..., sep = "___") {
  .df %>%
    dplyr::mutate(variable = paste(variable, construct, sep = sep)) %>%
    dplyr::group_by(construct) %>%
    score_control(score = score) %>%
    dplyr::arrange(construct, desc(score)) %>%
    dplyr::mutate(variable = fct_inorder(variable))
}

# from tidytext
scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
facet_constructs <- function() {
  ggplot2::facet_grid(. ~ construct, scales = "free_x", space = "free_x")
}
