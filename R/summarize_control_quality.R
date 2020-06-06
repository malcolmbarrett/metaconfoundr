#' Title
#'
#' @param .df
#' @param ...
#' @param domains
#'
#' @return
#' @export
#'
#' @examples
summarize_control_quality <- function(.df, ..., domains = TRUE) {
  adequate_context <- build_filter_quosures(...)
  domain_names <- names(adequate_context)
  adequate_studies <- purrr::map(adequate_context, pull_studies, .df = .df) %>%
    purrr::set_names(domain_names)

  partial_context <- build_filter_quosures(..., equal_to = "unclear")
  partial_studies <- purrr::map(partial_context, pull_studies, .df = .df) %>%
    purrr::set_names(domain_names)

  map_df <- tibble::tibble(
    domain = domain_names,
    adequate_studies,
    partial_studies
  )

  domain_df <- purrr::pmap_dfc(map_df, select_studies, .df = .df)

  control_df <- process_domains(domain_df, .df)

  if (!domains) return(dplyr::filter(control_df, variable == "overall"))

  control_df
}

process_domains <- function(domain_df, .df) {
  domain_df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(overall = all_controlled(dplyr::c_across())) %>%
    dplyr::select(overall, dplyr::everything()) %>%
    dplyr::bind_cols(dplyr::select(.df, study), .) %>%
    tidyr::pivot_longer(-study, names_to = "variable", values_to = "control_quality") %>%
    dplyr::mutate(
      variable = set_variable_factor(variable),
      control_quality = set_control_factor(control_quality),
      construct = set_construct_factor(variable)
    )
}

all_controlled <- function(x) {
  dplyr::case_when(
    all(x == "adequate") ~ "adequate",
    all(x == "inadequate") ~ "inadequate",
    TRUE ~ "partial control"
  )
}

set_construct_factor <- function(x) {
  ordered(
    ifelse(x == "overall", "overall", "domains"),
    levels = c("overall", "domains")
  )
}

set_control_factor <- function(x) {
  ordered(x, levels = c("inadequate", "partial control", "adequate"))
}

set_variable_factor <- function(x) {
  domains <- unique(x)
  domains <- domains[domains != "overall"]

  ordered(x, levels = c("overall", domains))
}

build_filter_quosures <- function(..., equal_to = "adequate") {
  .dots <- rlang::enquos(...)

  dot_names <- names(.dots)
  names_missing <- dot_names == ""
  if (any(names_missing)) {
    domain_names <- paste0("domain", seq_along(dot_names))
    dot_names[names_missing] <- domain_names[names_missing]
  }

  variables_regex <- "(\\`.*?\\`|[:word:]+)"
  replacement_predicate <- paste0("\\1 == \\'", equal_to, "\\'")
  logic_text <- purrr::map_chr(.dots, rlang::quo_text) %>%
    stringr::str_replace_all(variables_regex, replacement_predicate)

  logic_text <- rlang::parse_exprs(logic_text)
  names(logic_text) <- dot_names

  logic_text
}


select_studies <- function(domain, adequate_studies, partial_studies, .df) {
  .df %>%
    dplyr::mutate({{domain}} := dplyr::case_when(
      study %in% adequate_studies ~ "adequate",
      study %in% partial_studies ~ "partial control",
      TRUE ~ "inadequate"
    )) %>%
    dplyr::select({{domain}})
}


pull_studies <- function(context, .df) {
  .x <- .df %>%
    dplyr::select(study, variable, control_quality) %>%
    tidyr::pivot_wider(names_from = variable, values_from = control_quality) %>%
    dplyr::filter(!!context)

  .x[["study"]]
}



