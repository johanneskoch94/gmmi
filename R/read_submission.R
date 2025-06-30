check_submission_file <- function(file, verbose = FALSE) {
  is_xlsx <- grepl("\\.xlsx$", file)
  if (!is_xlsx) {
    rlang::abort(glue::glue("\"{file}\" is not an xlsx file."))
  }

  sheets <- readxl::excel_sheets(file)
  if (verbose) {
    cli::cli_alert_success("The file \"{file}\" is the correct file type: \"xlsx\"")
    cli::cli_alert_success(
      "It has {length(sheets)} sheets: {glue::glue_collapse(glue::double_quote(sheets), ', ', last = ' and ')}."
    )
  }
}

read_submission <- function(file, verbose = FALSE) {
  if (length(file) > 1) {
    z <- purrr::map(file, ~read_submission(.x, verbose = verbose)) |>
      `names<-`(file) |>
      purrr::list_rbind(names_to = "File")
    return(z)
  }

  if (verbose) cli::cli_alert("Reading submission file: \"{file}\"")

  check_submission_file(file, verbose = verbose)

  sheets <- readxl::excel_sheets(file)
  y <- purrr::map(sheets, \(sheet) {
    x <- readxl::read_xlsx(file, sheet = sheet, .name_repair = "unique_quiet", progress = FALSE)

    if (!all(c("Variable", "Unit", "Region") %in% colnames(x)) || !any(grepl("^2", colnames(x)))) {
      if (verbose) cli::cli_alert_warning("Skipping sheet \"{sheet}\" (no data - most likely metadata)")
      return(NULL)
    } else {
      if (verbose) cli::cli_alert_success("Loading sheet \"{sheet}\"")
    }

    x |>
      dplyr::filter(!is.na(.data$Unit),
                    !is.na(.data$Region),
                    !dplyr::if_all(tidyselect::matches("^2\\d\\d\\d$"), is.na)) |>
      dplyr::select(-tidyselect::where(~ all(is.na(.))), -tidyselect::starts_with("...")) |>
      dplyr::distinct() |>
      tidyr::pivot_longer(tidyselect::matches("^2\\d\\d\\d$"), names_to = "Year")
  }) |>
    `names<-`(sheets) |>
    purrr::list_rbind(names_to = "Sheet")

  if (dplyr::n_distinct(y) == dplyr::n_distinct(dplyr::select(y, -"Sheet"))) {
    y <- dplyr::select(y, -"Sheet")
  } else {
    if (verbose) cli::cli_alert_warning("Data on different sheets not uniquely specified by default - \\
                                        use the \"Sheet\" column to differentiate.")
  }

  if (verbose) cli::cli_text(" ")
  if (verbose) cli::cli_alert_info("Summary of data:")

  non_template_columns <- colnames(y)[! colnames(y) %in% c("Sheet", colnames(read_template()))]
  if (!rlang::is_empty(non_template_columns)) {
    y <- y |> dplyr::select(-non_template_columns) |> dplyr::distinct()
    if (verbose) cli::cli_alert_warning(
      "Dropping unknown columns: {glue::glue_collapse(glue::double_quote(non_template_columns), ', ', last = ' and ')}"
    )
  }

  common_vars <- y |>
    dplyr::select(c("Variable", "Definition", "Unit")) |>
    dplyr::distinct() |>
    dplyr::semi_join(get_template_variables(), by = dplyr::join_by("Variable", "Definition", "Unit"))
  diff_vars <- y |>
    dplyr::select(c("Variable", "Definition", "Unit")) |>
    dplyr::distinct() |>
    dplyr::anti_join(get_template_variables(), by = dplyr::join_by("Variable", "Definition", "Unit"))

  y <- y |>
    dplyr::semi_join(get_template_variables(), by = dplyr::join_by("Variable", "Definition", "Unit"))

  scen <- unique(y$Scenario)
  if (verbose) cli::cli_bullets(c(
    "*" = "Data for following scenarios: {glue::glue_collapse(glue::double_quote(scen), ', ', last = ' and ')}"
  ))

  reg <- unique(y$Region)
  if (verbose) cli::cli_bullets(c(
    "*" = "Data for following regions: {glue::glue_collapse(glue::double_quote(reg), ', ', last = ' and ')}"
  ))

  years <- unique(y$Year)
  if (verbose) cli::cli_bullets(c(
    "*" = "Data for following years: {glue::glue_collapse(years, ', ', last = ' and ')}"
  ))

  n_vars <- length(common_vars$Variable)
  n_vars_tot <- length(get_template_variables()$Variable)
  if (verbose) cli::cli_bullets(c(
    "*" = "Number of template variables reported: {n_vars}/{n_vars_tot}."
  ))

  n_vars_extra <- length(diff_vars$Variable)
  if (verbose) cli::cli_bullets(c(
    "*" = "Number of non-template variables reported: {n_vars_extra}."
  ))

  if (verbose) cli::cli_text(" ")

  y
}
