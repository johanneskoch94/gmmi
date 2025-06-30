#' Read-in and tidy the reporting template.
#'
#' read_template reads-in the template file (shipped with the package), and performs minor operations to tidy up
#' the data.
#'
#' @return Tidy data-frame with the reporting template
#' @export
read_template <- function() {
  readxl::read_xlsx(path = system.file("gmmi_template_241121.xlsx", package = "gmmi"),
                    sheet = 2,
                    range = "B1:Y70") |>
    tidyr::pivot_longer(tidyselect::starts_with("2"), names_to = "year")
}

#' Get template variable
#'
#' get_template_variables returns the template variable names and definitions.
#'
#' @param return_only_names If TRUE (default set to FALSE), return vector of variable names.
#'
#' @return Data-frame with the reporting template variables
#' @export
get_template_variables <- function(return_only_names = FALSE) {
  x <- read_template() |>
    dplyr::select(c("Variable", "Definition", "Unit")) |>
    dplyr::filter(!is.na(.data$Definition)) |>
    dplyr::distinct()

  if (return_only_names) x <- dplyr::pull(x, "Variable")

  x
}
