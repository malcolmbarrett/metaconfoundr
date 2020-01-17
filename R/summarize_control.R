control2 <- function(.variable, control_level = c("adequate", "unclear")) {
  control_level <- match.arg(control_level)
  expr <- paste0("`", .variable, "`", " == '", control_level, "'")
  expr <- rlang::sym(expr)
  expr
}

#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
summarize_control <- function(...) {

}

adequate <- function() {
  "adequate"
}

unclear <- function() {
  "unclear"
}


pivot_mc2 <- function(.x) {
  .x %>%
    dplyr::select(study, variable, control_quality) %>%
    tidyr::pivot_wider(names_from = variable, values_from = control_quality)
}
