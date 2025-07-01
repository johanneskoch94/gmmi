#' Plot GMMI variables
#'
#' plot_over_time creates a plot of a GMMI variable over time.
#'
#' @param data data-frame with data
#' @param variable string with the variable name to plot
#'
#' @return ggplot
#' @export
plot_over_time <- function(data, variable) {
  rlang::check_installed("ggplot2")
  data |>
    dplyr::filter(.data$Variable == tidyselect::all_of(variable)) |>
    ggplot2::ggplot()+
    ggplot2::geom_line(ggplot2::aes(.data$Year, .data$value, colour = .data$File, linetype = .data$Scenario))+
    ggplot2::geom_point(ggplot2::aes(.data$Year, .data$value, colour = .data$File))+
    ggplot2::facet_wrap(~.data$Region, scales = "free_y")+
    ggplot2::ylab(variable)
}

#' @describeIn plot_over_time plot relative difference between scenarios over diff in emissions
#' @export
plot_diff_over_emi <- function(data, variable = "GDP") {
  rlang::check_installed(c("ggplot2", "scales"))
  data |>
    dplyr::select("File", "Variable", "Region", "Year", "Scenario", "value") |>
    tidyr::pivot_wider(names_from = "Scenario") |>
    dplyr::mutate(diff = (.data$`Emission reduction` - .data$Baseline) / .data$Baseline,
                  .keep = "unused") |>
    tidyr::pivot_longer("diff", names_to = "Scenario") |>
    dplyr::filter(.data$Variable %in% c("Emissions", tidyselect::all_of(variable))) |>
    tidyr::pivot_wider(names_from = "Variable") |>
    tidyr::unite("model_reg", c("File", "Region"), remove = FALSE) |>
    tidyr::drop_na(variable, "Emissions") |>
    ggplot2::ggplot()+
    ggplot2::geom_line(ggplot2::aes(-.data$Emissions, .data$GDP, colour = .data$model_reg))+
    ggplot2::geom_point(ggplot2::aes(-.data$Emissions, .data$GDP, colour = .data$model_reg))+
    ggplot2::scale_y_continuous(labels = scales::percent)+
    ggplot2::scale_x_continuous(labels = scales::percent)
}

#' @describeIn plot_over_time plot carbon price over diff in emissions
#' @param years years with which to filter the data before plotting
#' @export
plot_carbon_prive_over_emi <- function(data, years = 2025:2050) {
  rlang::check_installed(c("ggplot2", "scales"))

  cp <- dplyr::tibble("Year" = 2020:2050, "Carbon Price" = 0) |>
    dplyr::mutate(`Carbon Price` = 130 * 1.05^(.data$Year - 2020))

  data |>
    dplyr::select("File", "Variable", "Region", "Year", "Scenario", "value") |>
    tidyr::pivot_wider(names_from = "Scenario") |>
    dplyr::mutate(diff = (.data$`Emission reduction` - .data$Baseline) / .data$Baseline,
                  .keep = "unused") |>
    tidyr::pivot_longer("diff", names_to = "Scenario") |>
    dplyr::filter(.data$Variable %in% c("Emissions"), .data$Year %in% tidyselect::all_of(years)) |>
    tidyr::pivot_wider(names_from = "Variable") |>
    tidyr::unite("model_reg", c("File", "Region"), remove = FALSE) |>
    dplyr::left_join(cp, by = "Year") |>
    tidyr::drop_na("Emissions", "Carbon Price") |>
    ggplot2::ggplot()+
    ggplot2::geom_line(ggplot2::aes(-.data$Emissions, .data$`Carbon Price`, colour = .data$model_reg))+
    ggplot2::geom_point(ggplot2::aes(-.data$Emissions, .data$`Carbon Price`, colour = .data$model_reg))+
    ggplot2::scale_x_continuous(labels = scales::percent)+
    ggplot2::scale_y_continuous(labels = scales::dollar)
}
