#' Read-in and tidy the reporting template.
#'
#' read_template reads-in the template file (shipped with the package), and performs minor operations to tidy up
#' the data.
#'
#' @param sheet Name or number of sheet to read in.
#'
#' @return Tidy data-frame with the reporting template
#' @export
read_template <- function(sheet = "Baseline template") {
  readxl::read_xlsx(path = system.file("gmmi_template_241121.xlsx", package = "gmmi"),
                    sheet = sheet,
                    range = "B1:Y70",
                    .name_repair = "unique_quiet",
                    progress = FALSE) |>
    dplyr::filter(!is.na(.data$Definition)) |>
    tidyr::pivot_longer(tidyselect::matches("2\\d\\d\\d"), names_to = "Year")
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
    dplyr::distinct(.data$Variable, .data$Definition, .data$Unit)

  if (return_only_names) x <- dplyr::pull(x, "Variable")

  x
}

#' Get template scenarios
#'
#' get_template_scenarios returns the template scenario names.
#'
#' @return Vector of strings
#' @export
get_template_scenarios <- function() {
  c(unique(read_template()$Scenario),
    unique(read_template(sheet = "Emission reduction template")$Scenario))
}
