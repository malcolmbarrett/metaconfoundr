#' Summarize the control quality of studies
#'
#' `summarize_control_quality()` allows you to summarize how well studies
#' control for variables within one or more domains, and how well those domains
#' are controlled for overall. Each logical statement is a domain and can be
#' named.
#'
#' @param .df A data frame, usually the result of [`metaconfoundr()`]
#' @param ... Boolean arguments to declare adequate control logic
#' @param domains Logical. Include the domains in the output? If `FALSE`, only
#'   returns overall control quality.
#'
#' @return A tibble
#' @export
#'
#' @examples
#'
#' summary_df <- summarize_control_quality(
#'   metaconfoundr(ipi),
#'   Sociodemographics = `Maternal age` & `Race/ethnicity` & `Marital status`,
#'   Socioeconomics = `SES category` | Insurance & Education,
#'   "Reproductive Hx" = `Prior pregnancy outcome`
#' )
#'
#' summary_df
#'
#' summary_df %>%
#'   mc_trafficlight() +
#'   theme_mc() +
#'   facet_constructs() +
#'   geom_cochrane() +
#'   scale_fill_cochrane()
#'
summarize_control_quality <- function(.df, ..., domains = TRUE) {
  adequate_context <- build_filter_quosures(...)
  domain_names <- names(adequate_context)
  adequate_studies <- purrr::map(adequate_context, pull_studies, .df = .df) %>%
    purrr::set_names(domain_names)

  partial_context <- build_filter_quosures(..., equal_to = "some concerns")
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
    dplyr::bind_cols(dplyr::distinct(.df, study), .) %>%
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
    TRUE ~ "some concerns"
  )
}

set_construct_factor <- function(x) {
  factor(
    ifelse(x == "overall", "overall", "domains"),
    levels = c("overall", "domains")
  )
}

set_control_factor <- function(x) {
  ordered(x, levels = c("inadequate", "some concerns", "adequate"))
}

set_variable_factor <- function(x) {
  domains <- unique(x)
  domains <- domains[domains != "overall"]

  factor(x, levels = c("overall", domains))
}

build_filter_quosures <- function(..., equal_to = "adequate") {
  quo_dots <- rlang::enquos(...)

  if (is_first_quo_string(quo_dots)) quo_dots <- string_to_quosure(quo_dots)

  dot_names <- names(quo_dots)
  names_missing <- dot_names == ""
  if (any(names_missing)) {
    domain_names <- paste0("domain", seq_along(dot_names))
    dot_names[names_missing] <- domain_names[names_missing]
  }

  variables_regex <- "(\\`.*?\\`|[:word:]+)"
  replacement_predicate <- paste0("\\1 == \\'", equal_to, "\\'")
  logic_text <- purrr::map_chr(quo_dots, rlang::quo_text) %>%
    stringr::str_replace_all(variables_regex, replacement_predicate)

  logic_text <- rlang::parse_exprs(logic_text)
  names(logic_text) <- dot_names

  logic_text
}

is_first_quo_string <- function(x) {
  first_quo <- x[[1]]
  tryCatch(
    rlang::is_string(rlang::eval_tidy(first_quo)),
    error = function(e) FALSE
  )
}

string_to_quosure <- function(x) {
  x <- rlang::eval_tidy(x[[1]])
  x <- stringr::str_split(x, "\\s*,\\s*")[[1]]
  x_names_regex <- '[:punct:]?[\\w\\s]*[:punct:]?\\s*=\\s*'
  x_names <- stringr::str_extract(x, x_names_regex) %>%
    stringr::str_remove("\\s*=\\s*") %>%
    stringr::str_remove_all("\'|\"")

  x <- stringr::str_split(x, x_names_regex, simplify = TRUE)[, 2]
  x_quos <- purrr::map(
    rlang::parse_exprs(x),
    rlang::as_quosure,
    env = rlang::caller_env()
  )

  names(x_quos) <- x_names

  rlang::as_quosures(x_quos, env = rlang::caller_env())
}


select_studies <- function(domain, adequate_studies, partial_studies, .df) {
  .df %>%
    dplyr::distinct(study) %>%
    dplyr::mutate({{domain}} := dplyr::case_when(
      study %in% adequate_studies ~ "adequate",
      study %in% partial_studies ~ "some concerns",
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



