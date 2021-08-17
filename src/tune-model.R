# Tuning forecasting model
# Selects best weighting on ETS and ARIMA

# *****************************************************************************
# Setup ----

# Data series to use
series <- "inflation"
latest_data <- "2021Q2"

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
# Load data ---- 

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

last_actual <- dat_model |> 
  mutate(rn = row_number()) |> 
  filter(date == yearquarter("2016Q1")) |> 
  pull(rn)

# *****************************************************************************


# *****************************************************************************
# Model tuning ---- 

# Estimate and test accuracy of weighted ensemble model
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

# Run tests 
tests <- tibble(
  w = seq(from = 0, to = 1, by = 0.05)
) |> 
  rowwise() |> 
  mutate(accuracy = list(test_ensemble(d = dat_model, 
                                       w = w, 
                                       start = last_actual))) |> 
  unnest(cols = accuracy)

tests |> 
  ggplot(mapping = aes(x = w, 
                       y = MAPE)) + 
  geom_col() + 
  scale_x_continuous(breaks = seq(0, 1, 0.05))
