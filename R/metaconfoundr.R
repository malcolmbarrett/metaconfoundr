#' Title
#'
#' @param .df
#' @param data_format
#'
#' @return
#' @export
#'
#' @examples
metaconfoundr <- function(.df, data_format = mc_detect_layout()) {
  data_format(.df)
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
mc_detect_layout <- function(...) {
  function(.df) {
    if (ncol(.df) > 5) return(mc_wider(...)(.df))
    mc_longer(...)(.df)
  }
}

#' Title
#'
#' @param study
#' @param construct
#' @param variable
#' @param control_quality
#' @param is_confounder
#' @param study_values
#'
#' @return
#' @export
#'
#' @examples
mc_longer <- function(
  study = contains("construct"),
  construct = contains("construct"),
  variable = matches("variable|factor"),
  control_quality = contains("control_quality"),
  is_confounder = contains("confounder"),
  study_values = mc_study_values()
) {
  function(.df) {
    nms <- names(.df)
    study <- validate_select(nms, study)
    construct <- validate_select(nms, construct)
    variable <- validate_select(nms, variable)
    control_quality <- validate_select(nms, control_quality)
    is_confounder <- validate_select(nms, is_confounder)

    .df %>%
      dplyr::rename(
        study = study,
        construct = construct,
        variable = variable,
        control_quality = control_quality,
        is_confounder = is_confounder
      ) %>%
      dplyr::mutate(
        control_quality = ordered(
          control_quality,
          levels = study_values,
          labels = names(study_values)
        )
      )
  }
}

#' Title
#'
#' @param inadequate
#' @param unclear
#' @param adequate
#'
#' @return
#' @export
#'
#' @examples
mc_study_values <- function(inadequate = 0, unclear = 1, adequate = 2) {
  c(inadequate = inadequate, unclear = unclear, adequate = adequate)
}

#' Title
#'
#' @param construct
#' @param variable
#' @param is_confounder
#' @param study
#' @param study_values
#'
#' @return
#' @export
#'
#' @examples
mc_wider <- function(
  construct = contains("construct"),
  variable = matches("variable|factor"),
  is_confounder = contains("confounder"),
  study = everything(),
  study_values = mc_study_values()
) {
  function(.df) {
    nms <- names(.df)
    columns <- tidyselect::vars_select(nms, -construct, -variable, -is_confounder) %>%
      tidyselect::vars_select(study)

    .df <- tidyr::pivot_longer(
      .df,
      cols = columns,
      names_to = "study",
      values_to = "control_quality"
    )

    prep_df <- mc_longer(construct = construct, variable = variable, is_confounder = is_confounder, study_values = study_values)
    prep_df(.df)
  }
}

validate_select <- function(nms, x) {
  tryCatch(
    tidyselect::vars_select(nms, {{x}}),
    error = function(e) stop(paste0("Couldn't find column `", get_text(x) ,"`. Use `mc_longer()` or mc_wider()` to identify columns explicitly."), call. = FALSE)
  )
}

get_text <- function(x) rlang::quo_text(rlang::enquo(x))


