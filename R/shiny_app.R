#' Launch metaconfoundr Shiny app
#'
#' `launch_metaconfoundr_app()` launches a Shiny app to create visualizations of
#' confounding control in meta-analyses
#'
#' @export
#' @return A Shiny app
launch_metaconfoundr_app <- function() {
  app_dir <- system.file("shiny_app", package = "metaconfoundr")
  if (app_dir == "") {
    stop("Shiny app not found. Try re-installing `metaconfoundr`.", call. = FALSE)
  }

  shiny::runApp(app_dir, display.mode = "normal")
}
