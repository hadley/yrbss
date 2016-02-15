library(readr)
library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)
library(stringr)

vars <- read_csv("data-raw/variables.csv")
df <- read_fwf(
  "data-raw/sadc_2013_national.dat",
  col_positions = fwf_positions(vars$start, vars$end, vars$variable)
)

# Factor levels -----------------------------------------------------------
parse_group <- function(lines) {
  var <- lines[2] %>% str_trim() %>% str_to_lower()

  pieces <- lines[-(1:2)] %>% str_split_fixed(" ", 2)
  levels <- pieces[, 1]
  labels <- pieces[, 2] %>% str_replace_all("['\"]", "")

  label <- function(x) {
    factor(x, levels = levels, labels = labels)
  }

  set_names(list(label), var)
}

levels <- read_lines("data-raw/levels.txt")
grp <- cumsum(levels == "/")

factorise <- levels %>%
  split(grp) %>%
  map(parse_group) %>%
  flatten()
factorise <- factorise[intersect(names(factorise), names(df))]

survey <- df
for (var in names(factorise)) {
  cat(".")
  survey[[var]] <- factorise[[var]](survey[[var]])
}


# Replace numeric 0's with NA ---------------------------------------------

num_vars <- survey %>% map_lgl(is_numeric)
replace_0 <- function(x) {
  x[x == 0] <- NA
  x
}

survey[num_vars] <- survey[num_vars] %>% map(replace_0)

# Variable labels ---------------------------------------------------------

survey[] <- map2(survey, vars$label, function(x, label) {
  if (is.na(label)) {
    x
  } else {
    structure(x, label = label)
  }
})
