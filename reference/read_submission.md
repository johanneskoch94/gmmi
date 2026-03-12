# Read in data from GMMI submissions

Read in data from one or multiple GMMI submissions.

## Usage

``` r
read_submission(
  file,
  model_alias_mapping = NULL,
  harmonize_regions = TRUE,
  harmonize_scenarios = TRUE,
  verbose = FALSE,
  only_template_var = TRUE,
  var_match_strict = TRUE,
  save_log = FALSE
)
```

## Arguments

- file:

  Path or vector of file paths.

- model_alias_mapping:

  If a valid file path (NULL by default), use model aliases contained in
  the file instead of model names.

- harmonize_regions:

  IF TRUE (default), rename regions to iso3c codes and drop unknown
  (keeping only countries and certain aggregates (GLO, EU27).

- harmonize_scenarios:

  IF TRUE (default), rename scenarios to conform to template.

- verbose:

  If TRUE (FALSE by default), print information to screen.

- only_template_var:

  IF TRUE (default), keep only template variables

- var_match_strict:

  IF TRUE (default), variable have to match in their name, unit and
  definition fields, to that of the template's.

- save_log:

  If TRUE (FALSE by default), redirect output to "read_submission.log"

## Value

data-frame with submission data
