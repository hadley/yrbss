library(readr)
library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)

vars <- read_csv("data-raw/variables.csv")
df <- read_fwf(
  "data-raw/sadc_2013_national.dat",
  col_positions = fwf_positions(vars$start, vars$end, vars$variable)
)

# Add labels
df[] <- map2(df, vars$label, function(x, label) {
  if (is.na(label)) {
    x
  } else {
    structure(x, label = label)
  }
})
