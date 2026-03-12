#' Read in data from GMMI submissions
#'
#' Read in data from one or multiple GMMI submissions.
#'
#' @param file Path or vector of file paths.
#' @param model_alias_mapping  If a valid file path (NULL by default), use model aliases contained in the file instead
#'   of model names.
#' @param harmonize_regions IF TRUE (default), rename regions to iso3c codes and drop unknown (keeping only countries
#'   and certain aggregates (GLO, EU27).
#' @param harmonize_scenarios IF TRUE (default), rename scenarios to conform to template.
#' @param verbose If TRUE (FALSE by default), print information to screen.
#' @param only_template_var IF TRUE (default), keep only template variables
#' @param var_match_strict IF TRUE (default), variable have to match in their name, unit and definition fields, to that
#'   of the template's.
#' @param save_log If TRUE (FALSE by default), redirect output to "read_submission.log"
#'
#' @return data-frame with submission data
#' @export
read_submission <- function(file,
                            model_alias_mapping = NULL,
                            harmonize_regions = TRUE,
                            harmonize_scenarios = TRUE,
                            verbose = FALSE,
                            only_template_var = TRUE,
                            var_match_strict = TRUE,
                            save_log = FALSE) {
  if (save_log) {
    file.create("read_submission.log")
    withr::local_message_sink("read_submission.log", append = TRUE)
  }

  if (length(file) > 1) {
    z <- purrr::map(file, purrr::possibly(
      ~ read_submission(.x,
                        model_alias_mapping = model_alias_mapping,
                        harmonize_regions = harmonize_regions,
                        harmonize_scenarios = harmonize_scenarios,
                        verbose = verbose,
                        only_template_var = only_template_var,
                        var_match_strict = var_match_strict),
      quiet = !verbose)
    )

    if (verbose) {
      cli::cli_text(" ")
      cli::cli_alert("Summary: ")
      purrr::walk2(file, z, \(x, y) {
        if (!is.null(y)) {
          cli::cli_alert_success("{basename(x)}")
        } else {
          cli::cli_alert_danger("{basename(x)}")
        }
      })
    }


    z <- z |>
      purrr::discard(is.null) |>
      purrr::list_rbind() |>
      dplyr::relocate("value", .after = dplyr::last_col())

    return(z)
  }

  if (verbose) cli::cli_text(" ")
  if (verbose) cli::cli_alert("Reading submission file: \"{file}\"")

  check_submission_file(file, verbose = verbose)
  model_name <- sub("\\.xlsx", "", sub("GMMI_", "", basename(file)))
  if (!is.null(model_alias_mapping)) {
    model_name <- get_model_alias(model_alias_mapping, model_name)
  }

  # Loop over sheets and read in data
  sheets <- readxl::excel_sheets(file)
  y <- purrr::map(sheets, \(sheet) {
    x <- readxl::read_xlsx(file, sheet = sheet, .name_repair = "unique_quiet", progress = FALSE)

    if (!any(grepl("^(V|v)ariab", colnames(x))) ||
        !any(grepl("^Region|country", colnames(x))) ||
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
    if ("country" %in% colnames(x)) {
      if (verbose) cli::cli_alert_warning("Renaming \"country\" to \"Region\".")
      x <- dplyr::rename(x, "Region" = "country")
    }
    if ("scenario" %in% colnames(x)) {
      if (verbose) cli::cli_alert_warning("Renaming \"scenario\" to \"Scenario\".")
      x <- dplyr::rename(x, "Scenario" = "scenario")
    }

    if (any(grepl("^\\.\\.\\.", colnames(x)))) {
      if (verbose) cli::cli_alert_warning("Dropping columns without headers.")
      x <- dplyr::select(x, -tidyselect::starts_with("..."))
    }

    # Add specific corrections
    if (basename(file) == "GMMI_U Indonesia_v2.xlsx" && "Region" %in% colnames(x)) {
      x <- dplyr::mutate(x, Region = "Indonesia")
    }
    if (basename(file) == "GMMI_NCAER_v3.xlsx" && "Region" %in% colnames(x)) {
      x <- dplyr::mutate(x, Region = "India")
    }
    if (basename(file) == "GMMI_WB-ME.xlsx" && "Region" %in% colnames(x)) {
      x <- dplyr::mutate(x, Region = "unknown")
    }
    if (basename(file) == "GMMI_RITE.xlsx" && sheet == "Emission reduction template") {
      x <- dplyr::mutate(x, Scenario = "Emission reduction")
    }

    x <-  x |>
      tidyr::pivot_longer(tidyselect::matches("^2\\d\\d\\d$"),
                          names_to = "Year",
                          names_transform = as.integer,
                          values_transform = as.numeric) |>
      suppressWarnings() |>
      tidyr::drop_na("Scenario", "Region", "value") |>
      dplyr::distinct()

    if (nrow(x) == 0) {
      if (verbose) cli::cli_alert_warning("No data found on sheet.")
      return(x)
    }

    x |>
      dplyr::mutate(dplyr::across(tidyselect::matches("(i|I)nput|(o|O)u(|t)put"), ~dplyr::case_match(
        as.character(.x),
        c("Yes", "yes", "x", "X", 1, "TRUE") ~ TRUE,
        .default = FALSE
      ) |> as.logical()))
  }) |>
    `names<-`(sheets)|>
    purrr::compact() |>
    purrr::discard(~nrow(.x) == 0)

  # Check if some data exitst, and combine sheets
  if (rlang::is_empty(y)) {
    cli::cli_alert_danger("No data found in file.")
    return()
  }
  y <- y |>
    purrr::list_rbind(names_to = "Sheet") |>
    dplyr::mutate(Model = model_name, .before = 1)

  # Look at data
  if (verbose) cli::cli_text("---")
  if (verbose) cli::cli_alert_info("Summary of data:")

  # Scenarios
  scen <- unique(y$Scenario)
  if (verbose) cli::cli_bullets(c(
    "*" = "Data for following scenarios: {glue::glue_collapse(glue::double_quote(scen), ', ', last = ' and ')}"
  ))
  if (harmonize_scenarios && !identical(scen, get_template_scenarios())) {
    if (verbose) cli::cli_alert_warning("Renaming scenarios to conform with template.")
    y <- y |> dplyr::mutate(Scenario = dplyr::case_when(
      grepl("(R|r)eference|baseline", .data$Scenario) ~ "Baseline",
      grepl("Transition|(C|c)arbon|(E|e)mission", .data$Scenario) ~ "Emission reduction",
      .default = .data$Scenario
    ))
    scen <- unique(y$Scenario)
    if (verbose) cli::cli_bullets(c(
      "*" = "Data for following scenarios: {glue::glue_collapse(glue::double_quote(scen), ', ', last = ' and ')}"
    ))
  }
  if (dplyr::n_distinct(y) == dplyr::n_distinct(dplyr::select(y, -"Sheet"))) {
    y <- dplyr::select(y, -"Sheet")
  } else {
    if (verbose) cli::cli_alert_warning("Data on different sheets do not have unique scenario names. \\
                                        Attaching sheet name to scenario.")
    y <- tidyr::unite(y, "Scenario", c("Scenario", "Sheet")) |>
      dplyr::mutate(Scenario = sub("_Baseline template", "", .data$Scenario))
    scen <- unique(y$Scenario)
    if (verbose) cli::cli_bullets(c(
      "*" = "Data for following scenarios: {glue::glue_collapse(glue::double_quote(scen), ', ', last = ' and ')}"
    ))
  }


  # Regions
  reg <- unique(y$Region)
  if (verbose) cli::cli_bullets(c(
    "*" = "Data for following regions: {glue::glue_collapse(glue::double_quote(reg), ', ', last = ' and ')}"
  ))
  if (harmonize_regions && !all(reg %in% stats::na.omit(countrycode::codelist$iso3c))) {
    origin <-  if (any(nchar(reg) > 3)) "country.name" else "iso3c"
    y <- y |>
      dplyr::mutate(Region = dplyr::case_when(
        ! .data$Region %in% stats::na.omit(countrycode::codelist$iso3c) ~
          suppressWarnings(countrycode::countrycode(.data$Region, origin, "iso3c",
                                                    custom_match = c("Global" = "GLO",
                                                                     "EU" = "EU27",
                                                                     "EU27" = "EU27",
                                                                     "unknown" = "..."))),
        .default = .data$Region
      )) |>
      dplyr::filter(!is.na(.data$Region))

    reg <- unique(y$Region)
    if (verbose) {
      cli::cli_alert_warning("Renaming regions to iso3c codes, and dropping unknown (keeping only countries \\
                             and certain aggregates (GLO, EU27) for now.).")
      cli::cli_bullets(c(
        "*" = "Data for following regions: {glue::glue_collapse(glue::double_quote(reg), ', ', last = ' and ')}"
      ))
    }
  }
  if (rlang::is_empty(reg)) {
    rlang::abort(glue::glue("No valid regions."))
  }

  # Years
  years <- unique(y$Year)
  if (verbose) cli::cli_bullets(c(
    "*" = "Data for following years: {glue::glue_collapse(years, ', ', last = ' and ')}"
  ))
  if (rlang::is_empty(years)) {
    rlang::abort(glue::glue("No valid years."))
  }

  # Variables
  non_template_columns <- colnames(y)[! colnames(y) %in% c("Model", "Sheet", colnames(read_template()))]
  if (!rlang::is_empty(non_template_columns)) {
    y <- y |> dplyr::select(-non_template_columns) |> dplyr::distinct()
    if (verbose) cli::cli_alert_warning(
      "Dropping unknown columns: \\
      {glue::glue_collapse(glue::double_quote(non_template_columns), ', ', last = ' and ')}"
    )
  }

  var_mathc_cols <- if (var_match_strict) c("Variable", "Definition", "Unit") else "Variable"
  template_matcher <- colnames(y)[colnames(y) %in% var_mathc_cols]

  common_vars <- y |>
    dplyr::select(tidyselect::all_of(template_matcher)) |>
    dplyr::distinct() |>
    tidyr::drop_na() |>
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
                           the \\
                           {glue::glue_collapse(glue::double_quote(template_matcher), ', ', last = ' or ')} \\
                           {cli::qty(template_matcher)} column{?s}.")
  }

  diff_vars <- y |>
    dplyr::select(tidyselect::all_of(template_matcher)) |>
    dplyr::distinct() |>
    dplyr::anti_join(common_vars, by = template_matcher)

  if (only_template_var) {
    y <- y |>
      dplyr::semi_join(common_vars, by = template_matcher) |>
      tidyr::drop_na(.data$value)
  }

  n_vars <- length(common_vars$Variable)
  n_vars_tot <- length(get_template_variables()$Variable)
  if (verbose) cli::cli_bullets(c(
    "*" = "Number of template variables reported: {n_vars}/{n_vars_tot}, matched over \\
    {glue::glue_collapse(glue::double_quote(template_matcher), ', ', last = ' and ')}"
  ))

  n_vars_extra <- length(diff_vars$Variable)
  if (n_vars_extra != 0 && verbose) {
    cli::cli_bullets(c(
      "*" = "Number of non-template variables reported: {n_vars_extra}, matched over \\
    {glue::glue_collapse(glue::double_quote(template_matcher), ', ', last = ' and ')}"
    ))
    cli::cli_bullets(c(
      " " = "Non-template variables reported: \\
    {glue::glue_collapse(glue::double_quote(diff_vars$Variable), ', ', last = ' and ')}"
    ))
  }


  if (n_vars == 0 && n_vars_extra == 0) {
    rlang::abort(glue::glue("No valid variables."))
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
