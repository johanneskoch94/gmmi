# Plot GMMI variables

plot_over_time creates a plot of a GMMI variable over time.

## Usage

``` r
plot_over_time(data, variable)

plot_diff_over_emi(data, variable = "GDP")

plot_carbon_prive_over_emi(data, years = 2025:2050)
```

## Arguments

- data:

  data-frame with data

- variable:

  string with the variable name to plot

- years:

  years with which to filter the data before plotting

## Value

ggplot

## Functions

- `plot_diff_over_emi()`: plot relative difference between scenarios
  over diff in emissions

- `plot_carbon_prive_over_emi()`: plot carbon price over diff in
  emissions
