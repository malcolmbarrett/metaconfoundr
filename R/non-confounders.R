#' Count and plot non-confounders
#'
#' @param .df A data frame, usually the result of `metaconfoundr()`
#'
#' @return a `tibble` or ggplot
#' @export
#'
#' @examples
#'
#' ipi %>%
#'   metaconfoundr() %>%
#'   plot_non_confounders(size = 3, geom = ggplot2::geom_point)
#'
count_non_confounders <- function(.df) {
  no_values <- c("N", "NO", "FALSE", "0")
  .df %>%
    dplyr::group_by(study) %>%
    dplyr::summarise(
      n_non_confounders = sum(
        is_confounder %in% no_values & control_quality >= "some concerns",
        na.rm = TRUE
      ))
}

#' @param sort Logical. Should the results be sorted?
#' @param ... Arguments passed to `geom`
#' @param geom The ggplot2 geom to use
#'
#' @export
#'
#' @rdname count_non_confounders
plot_non_confounders <- function(.df, ..., geom = ggplot2::geom_col, sort = TRUE) {
  confounder_df <- .df %>%
    count_non_confounders()

  if (sort) {
    confounder_df <- confounder_df %>%
      dplyr::arrange(n_non_confounders) %>%
      dplyr::mutate(study = forcats::fct_inorder(study))
  }

  confounder_df %>%
    ggplot2::ggplot(ggplot2::aes(x = n_non_confounders, y = study)) +
    geom(...)
}
