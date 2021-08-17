# Tuning forecasting model
# Selects best weighting on ETS and ARIMA

# *****************************************************************************
# Setup ----

# Libraries
library(conflicted)
library(tidyverse)
library(glue)
library(here)
library(janitor)
library(scales)
library(fable)
library(feasts)
library(distributional)
library(lubridate)
library(tsibble)

# Conflicts
conflict_prefer(name = "filter", winner = "dplyr")
conflict_prefer(name = "lag", winner = "dplyr")

# Utility
source(here("src/utility.R"))
source(here("src/constants.R"))

# *****************************************************************************


# *****************************************************************************

series <- "inflation"
latest_data <- "2021Q2"
forecast_periods <- 8

dat <- read_csv(file = here(glue("data/{series}/{latest_data}/{series}.csv")), 
                col_types = "ccnccccc") |>
  clean_names() |> 
  filter(group == "CPI All Groups for New Zealand") |>
  separate(col = period, into = c("year", "quarter"), sep = "\\.", 
           convert = TRUE) |>
  mutate(quarter = as.integer(quarter / 3)) |> 
  mutate(date = yearquarter(glue("{year}Q{quarter}"))) |>
  filter(year > 1989) |> 
  select(date, year, quarter, cpi = data_value) |> 
  mutate(inflation_rate = cpi / lag(cpi, n = 4) - 1) |> 
  as_tsibble(index = date, regular = TRUE)

dat_model <- dat |> 
  select(date, inflation_rate) |> 
  filter(!is.na(inflation_rate)) |> 
  rename(value = inflation_rate)

test_ensemble <- function(d, w, start) {
  cat("* Testing ensemble with weight: ", w, "\n\n")
  
  if (w == 0.0) {
    m <- d |> 
      stretch_tsibble(.init = start, .step = 1) |> 
      model(
        test_model = 
          ETS(formula = value, ic = "bic")
      )
  } else if (w == 1.0) {
    m <- d |> 
      stretch_tsibble(.init = start, .step = 1) |> 
      model(
        test_model = 
          ARIMA(formula = value, ic = "bic")
      )
  } else {
    m <- d |> 
      stretch_tsibble(.init = start, .step = 1) |> 
      model(
        test_model = combination_model(
          ARIMA(formula = value, ic = "bic"), 
          ETS(formula = value, ic = "bic"), 
          cmbn_fn = combination_weighted, 
          cmbn_args = list(weights = c(w, 1 - w))
        )
      )
  }
  
  f <- m |> 
    forecast(h = 8) |> 
    group_by(.model, .id) |> 
    mutate(h = row_number()) |> 
    ungroup()
  
  a <- f |> 
    filter(date >= yearquarter("2018Q1"), 
           date <= yearquarter("2019Q4")) |> 
    accuracy(dat_model, 
             by = ".model")
  
  return(a)
}

tests <- tibble(
  w = c(0, 0.25, 0.5, 0.75, 1.0)
) |> 
  rowwise() |> 
  mutate(accuracy = list(test_ensemble(d = dat_model, 
                                       w = w, 
                                       start = 101))) |> 
  unnest(cols = accuracy)

tests <- tibble(
  w = c(0.2, 0.3, 0.4, 0.5, 0.6)
) |> 
  rowwise() |> 
  mutate(accuracy = list(test_ensemble(d = dat_model, 
                                       w = w, 
                                       start = 101))) |> 
  unnest(cols = accuracy)

tests <- tibble(
  w = c(0.3, 0.35, 0.4, 0.45, 0.5)
) |> 
  rowwise() |> 
  mutate(accuracy = list(test_ensemble(d = dat_model, 
                                       w = w, 
                                       start = 101))) |> 
  unnest(cols = accuracy)
