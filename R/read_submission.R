#' Read in data from GMMI submissions
#'
#' Read in data from one or multiple GMMI submissions.
#'
#' @param file Path or vector of file paths.
#' @param verbose If TRUE (FALSE by default), print information to screen.
#' @param save_log If TRUE (FALSE by default), redirect output to "read_submission.log"
#'
#' @return data-frame with submission data
#' @export
read_submission <- function(file, verbose = FALSE, save_log = FALSE) {
  if (save_log) {
    file.create("read_submission.log")
    withr::local_message_sink("read_submission.log", append = TRUE)
  }

  if (length(file) > 1) {
    z <- purrr::map(file, purrr::possibly(~read_submission(.x, verbose = verbose), quiet = F)) |>
      purrr::discard(is.null) |>
      purrr::list_rbind()
    return(z)
  }

  if (verbose) cli::cli_alert("Reading submission file: \"{file}\"")

  check_submission_file(file, verbose = verbose)
  model_name <-sub("\\.xlsx", "", sub("GMMI_", "", basename(file)))


  sheets <- readxl::excel_sheets(file)
  y <- purrr::map(sheets, \(sheet) {
    x <- readxl::read_xlsx(file, sheet = sheet, .name_repair = "unique_quiet", progress = FALSE)

    if (!any(grepl("^(V|v)ariab", colnames(x))) ||
        !any(grepl("^Region", colnames(x))) ||
        !any(grepl("^2", colnames(x)))) {
      if (verbose) cli::cli_alert_warning("Skipping sheet \"{sheet}\" (no data - most likely metadata)")
      return(NULL)
    } else {
      if (verbose) cli::cli_alert_success("Loading sheet \"{sheet}\"")
    }

    if ("Variabe" %in% colnames(x)) {
      if (verbose) cli::cli_alert_warning("Renaming \"Variabe\" to \"Variable\".")
      x <- dplyr::rename(x, "Variable" = "Variabe")
    }

    if (any(grepl("^\\.\\.\\.", colnames(x)))) {
      if (verbose) cli::cli_alert_warning("Dropping columns without headers.")
      x <- dplyr::select(x, -tidyselect::starts_with("..."))
    }

    x |>
      dplyr::filter(!is.na(.data$Region)) |>
      dplyr::distinct() |>
      tidyr::pivot_longer(tidyselect::matches("^2\\d\\d\\d$"),
                          names_to = "Year",
                          names_transform = as.integer,
                          values_transform = as.numeric) |>
      suppressWarnings() |>
      dplyr::filter(!all(is.na(.data$value)), .by = c("Variable", "Region")) |>
      dplyr::mutate(dplyr::across(tidyselect::matches("(i|I)nput|(o|O)u(|t)put"), ~dplyr::case_match(
        as.character(.x),
        c("Yes", "yes", "x", "X", 1, "TRUE") ~ TRUE,
        .default = FALSE
      ) |> as.logical()))
  }) |>
    `names<-`(sheets) |>
    purrr::list_rbind(names_to = "Sheet") |>
    dplyr::mutate(File = model_name, .before = 1)

  if (dplyr::n_distinct(y) == dplyr::n_distinct(dplyr::select(y, -"Sheet"))) {
    y <- dplyr::select(y, -"Sheet")
  } else {
    if (verbose) cli::cli_alert_warning("Data on different sheets not uniquely specified by default - \\
                                        use the \"Sheet\" column to differentiate.")
  }

  if (verbose) cli::cli_text(" ")
  if (verbose) cli::cli_alert_info("Summary of data:")

  non_template_columns <- colnames(y)[! colnames(y) %in% c("File", "Sheet", colnames(read_template()))]
  if (!rlang::is_empty(non_template_columns)) {
    y <- y |> dplyr::select(-non_template_columns) |> dplyr::distinct()
    if (verbose) cli::cli_alert_warning(
      "Dropping unknown columns: \\
      {glue::glue_collapse(glue::double_quote(non_template_columns), ', ', last = ' and ')}"
    )
  }

  template_matcher <- colnames(y)[colnames(y) %in% c("Variable", "Definition", "Unit")]

  common_vars <- y |>
    dplyr::select(tidyselect::all_of(template_matcher)) |>
    dplyr::distinct() |>
    fuzzyjoin::stringdist_join(get_template_variables(), by = template_matcher, mode = "semi")
  common_vars_strict <- y |>
    dplyr::select(tidyselect::all_of(template_matcher)) |>
    dplyr::distinct() |>
    dplyr::semi_join(get_template_variables(), by = template_matcher)

  if (verbose && !identical(common_vars, common_vars_strict)) {
    misspelled <- dplyr::anti_join(common_vars, common_vars_strict, by = template_matcher) |>
      dplyr::pull("Variable")
    cli::cli_alert_warning("The following variables contain spelling mistakes: \\
                           {glue::glue_collapse(glue::double_quote(misspelled), ', ', last = ' and ')} in \\
                           either the \\
                           {glue::glue_collapse(glue::double_quote(template_matcher), ', ', last = ' or ')} \\
                           columns.")
  }

  diff_vars <- y |>
    dplyr::select(tidyselect::all_of(template_matcher)) |>
    dplyr::distinct() |>
    fuzzyjoin::stringdist_join(get_template_variables(), by = template_matcher, mode = "anti")

  y <- y |>
    fuzzyjoin::stringdist_join(get_template_variables(), by = template_matcher, mode = "semi") |>
    tidyr::drop_na(.data$value)

  scen <- unique(y$Scenario)
  if (verbose) cli::cli_bullets(c(
    "*" = "Data for following scenarios: {glue::glue_collapse(glue::double_quote(scen), ', ', last = ' and ')}"
  ))
  if (!identical(scen, get_template_scenarios())) {
    if (verbose) cli::cli_alert_warning("Renaming scenarios to conform with template.")
    y <- y |> dplyr::mutate(Scenario = dplyr::case_match(
      .data$Scenario,
      c("Reference", "baseline") ~ "Baseline",
      c("Transition", "Carbon Price", "Emission Reduction") ~ "Emission reduction",
      .default = .data$Scenario
    ))
  }

  reg <- unique(y$Region)
  if (verbose) cli::cli_bullets(c(
    "*" = "Data for following regions: {glue::glue_collapse(glue::double_quote(reg), ', ', last = ' and ')}"
  ))
  if (!all(reg %in% stats::na.omit(countrycode::codelist$iso3c))) {
    origin <-  if (any(nchar(reg) > 3)) "country.name" else "iso3c"
    y <- y |>
      dplyr::mutate(Region = dplyr::case_when(
        ! .data$Region %in% stats::na.omit(countrycode::codelist$iso3c) ~
        suppressWarnings(countrycode::countrycode(.data$Region, origin, "iso3c",
                                                  custom_match = c("Global" = "GLO"))),
        .default = .data$Region
      )) |>
      dplyr::filter(!is.na(.data$Region))

    reg <- unique(y$Region)
    if (verbose) {
      cli::cli_alert_warning("Renaming regions to iso3c codes, and dropping unknown (keeping only countries \\
                             for now).")
      cli::cli_bullets(c(
        "*" = "Data for following regions: {glue::glue_collapse(glue::double_quote(reg), ', ', last = ' and ')}"
      ))
    }
  }

  years <- unique(y$Year)
  if (verbose) cli::cli_bullets(c(
    "*" = "Data for following years: {glue::glue_collapse(years, ', ', last = ' and ')}"
  ))

  n_vars <- length(common_vars$Variable)
  n_vars_tot <- length(get_template_variables()$Variable)
  if (verbose) cli::cli_bullets(c(
    "*" = "Number of template variables reported: {n_vars}/{n_vars_tot}, matched over \\
    {glue::glue_collapse(glue::double_quote(template_matcher), ', ', last = ' and ')}"
  ))

  n_vars_extra <- length(diff_vars$Variable)
  if (verbose) {
    cli::cli_bullets(c(
      "*" = "Number of non-template variables reported: {n_vars_extra}, matched over \\
    {glue::glue_collapse(glue::double_quote(template_matcher), ', ', last = ' and ')}"
    ))
    cli::cli_bullets(c(
      " " = "Non-template variables reported: \\
    {glue::glue_collapse(glue::double_quote(diff_vars$Variable), ', ', last = ' and ')}"
    ))
  }

  if (verbose) cli::cli_text(" ")

  y
}

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
