#' Add a score of confounding control
#'
#' `score_control()` adds a variable, `score`, that summarizes how well a study
#' controls for a domain or construct. Used to sort heatmaps and traffic light
#' plots.
#'
#' @param .df A data frame, usually the result of [metaconfoundr()]
#' @param score The approach used to calculate the score. `adequate` tests if
#'   the study controlled at a strictly adequate level. `sum` treats
#'   `control_quality` as an ordinal integer, summing it's values such that a
#'   higher score has better control overall. `controlled` tests if any control,
#'   including `some concerns` control, is present.
#'
#' @return a tibble
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' ipi %>%
#'   metaconfoundr() %>%
#'   filter(is_confounder == "Y") %>%
#'   score_control("controlled") %>%
#'   arrange(desc(score))
#'
score_control <- function(.df, score = c("adequate", "sum", "controlled")) {
  score <- match.arg(score)
  f <- get_score_f(score)

  .df %>%
    dplyr::group_by(variable, .add = TRUE) %>%
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

reorder_variable <- function(.df, score = c("adequate", "sum", "controlled"), ..., sep = "___") {
  .df %>%
    dplyr::mutate(variable = paste(variable, construct, sep = sep)) %>%
    dplyr::group_by(construct) %>%
    score_control(score = score) %>%
    dplyr::arrange(construct, dplyr::desc(score)) %>%
    dplyr::mutate(variable = forcats::fct_inorder(variable))
}

