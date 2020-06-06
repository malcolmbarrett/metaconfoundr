#' Title
#'
#' @param .df
#'
#' @return
#' @export
#'
#' @examples
count_confounders <- function(.df) {
  no_values <- c("N", "NO", "FALSE", "0")
  .df %>%
    dplyr::group_by(study) %>%
    dplyr::summarise(
      n_non_confounders = sum(
        is_confounder %in% no_values & control_quality >= "unclear",
        na.rm = TRUE
      ))
}

#' Title
#'
#' @param .df
#' @param sort
#' @param ...
#' @param geom
#'
#' @return
#' @export
#'
#' @examples
plot_non_confounders <- function(.df, sort = TRUE, ..., geom = ggplot2::geom_col) {
  confounder_df <- .df %>%
    count_confounders()

  if (sort) {
    confounder_df <- confounder_df %>%
      dplyr::arrange(n_non_confounders) %>%
      dplyr::mutate(study = forcats::fct_inorder(study))
  }

  confounder_df %>%
    ggplot2::ggplot(ggplot2::aes(x = n_non_confounders, y = study)) +
    geom(...)
}
