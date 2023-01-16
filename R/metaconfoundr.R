#' Prepare a meta-analysis data set for metaconfoundr
#'
#' `metaconfoundr()` standardizes data frames with information on how well a set
#' of studies control for a set of variables. In this approach, a set of domain
#' experts agree on the variables that are required to properly control for
#' confounding for a scientific question. Then, for a given confounder, the
#' studies are described as being adequately controlled, inadequately
#' controlled, or controlled with some concerns. `metaconfoundr()` is intended
#' to standardize data for use in [`mc_heatmap()`] and [`mc_trafficlight()`].
#' See the vignette on data preparation for more information on how to set up
#' your evaluation.
#'
#' @param .df A data frame. See the vignette on data preparation for more
#'   details.
#' @param data_format The format of the data. Detected automatically by default,
#'   but explicit options include [`mc_longer()`] and [`mc_wider()`]
#'
#' @return a tibble
#' @export
#'
#' @examples
#'
#' metaconfoundr(ipi)
#'
#' metaconfoundr(ipi_wide)
#'
#' ipi_wide2 <- ipi_wide %>%
#'   dplyr::rename(scope = construct)
#'
#' metaconfoundr(ipi_wide2, mc_wider(construct = "scope"))
#'
#'
#' @name metaconfoundr()
metaconfoundr <- function(.df, data_format = mc_detect_layout()) {
  data_format(.df)
}

#' Tidy metaconfoundr data layouts
#'
#' `mc_longer()` and `mc_wider()` are helper functions to put [metaconfoundr()]
#' for long and wide data sets, respectively. results into a tidy format.
#' `mc_detect_layout()` chooses between the two automatically based on the
#' number of variables in the data frame. `mc_study_values()` helps standardize
#' evaluations of control quality.
#'
#' @param ... Additional arguments passed to `mc_wider()` or `mc_longer()`
#'
#' @return a function that tidies the data
#' @export
mc_detect_layout <- function(...) {
  function(.df) {
    if (ncol(.df) > 5) return(mc_wider(...)(.df))
    mc_longer(...)(.df)
  }
}

#' @param study The column with the name of the studies
#' @param construct The domain or construct column
#' @param variable The column that describes the confounding variables
#' @param control_quality The column that describes the confounding control
#'   quality
#' @param is_confounder The column that describes if a variable is a confounder
#' @param study_values What are the levels of `control_quality`? Use
#'   `mc_study_values()` to set up.
#'
#' @export
#'
#' @rdname mc_detect_layout
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
    study <- validate_select(nms, dplyr::all_of(study))
    construct <- validate_select(nms, dplyr::all_of(construct))
    variable <- validate_select(nms, dplyr::all_of(variable))
    control_quality <- validate_select(nms, dplyr::all_of(control_quality))
    is_confounder <- validate_select(nms, dplyr::all_of(is_confounder))

    .df %>%
      dplyr::rename(
        study = unname(study),
        construct = unname(construct),
        variable = unname(variable),
        control_quality = unname(control_quality),
        is_confounder = unname(is_confounder)
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

#' @param inadequate Which value signifies inadequate control?
#' @param some_concerns Which value signifies control with some concerns?
#' @param adequate Which value signifies adequate control?
#'
#' @export
#'
#' @rdname mc_detect_layout
mc_study_values <- function(inadequate = 0, some_concerns = 1, adequate = 2) {
  c(inadequate = inadequate, "some concerns" = some_concerns, adequate = adequate)
}


#' @export
#' @rdname mc_detect_layout
mc_wider <- function(
  construct = contains("construct"),
  variable = matches("variable|factor"),
  is_confounder = contains("confounder"),
  study = everything(),
  study_values = mc_study_values()
) {
  function(.df) {
    nms <- names(.df)
    columns <- tidyselect::vars_select(
      nms,
      -dplyr::all_of(construct),
      -dplyr::all_of(variable),
      -dplyr::all_of(is_confounder)
    ) %>%
      tidyselect::vars_select(dplyr::all_of(study))
    columns <- unname(columns)

    .df <- tidyr::pivot_longer(
      .df,
      cols = dplyr::all_of(columns),
      names_to = "study",
      values_to = "control_quality"
    )

    prep_df <- mc_longer(
      construct = construct,
      variable = variable,
      is_confounder = is_confounder,
      study_values = study_values
    )

    prep_df(.df)
  }
}

validate_select <- function(nms, x) {
  error_msg <- paste0(
    "Couldn't find column `",
    get_text(x),
    "`. Use `mc_longer()` or mc_wider()` to identify columns explicitly."
  )

  tryCatch(
    tidyselect::vars_select(nms, {{x}}),
    error = function(e) stop(error_msg, call. = FALSE)
  )
}

get_text <- function(x) rlang::quo_text(rlang::enquo(x))


