# Forecasts of New Zealand's quarterly seasonally adjusted real GDP growth rate

# Created by aaron@schiff.nz
# https://github.com/aaronschiff/honest-economic-forecasts 

# Data updated to: 2021 Q1


# *****************************************************************************
# Setup ----

# Forecast configuration
series <- "gdp"
latest_data <- "2021Q1"
forecast_periods <- 8
forecast_uncertainty_reps <- 5000

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
library(as.charts)   # Custom library for formatting charts nicely
source(here("src/utility.R"))

# Conflicts
conflict_prefer(name = "filter", winner = "dplyr")
conflict_prefer(name = "lag", winner = "dplyr")

# *****************************************************************************


# *****************************************************************************
# Load data ----

# Read GDP dat aand select those for forecasting
dat <- read_csv(file = here(glue("data/{series}/{latest_data}/{series}.csv")), 
                col_types = "ccccnn") |>
  clean_names() |> 
  filter(level == "Total GDP") |>
  mutate(date = yearquarter(quarter)) |>
  mutate(growth_rate = amount / lag(amount) - 1) |> 
  as_tsibble(index = date, regular = TRUE)

# *****************************************************************************


# *****************************************************************************
# Forecast ----
# Values for 2020Q2 and Q3 are replaced with modelled (interpolated) values to
# prevent lockdown shock from affecting forecasts too much. 

# Data for 2020Q2 and Q3 interpolation
dat_model <- dat |> 
  mutate(growth_rate_to_interp = ifelse(quarter %in% c("2020Q2", "2020Q3"), 
                                        NA_real_, 
                                        growth_rate))

# Interpolated values
interpolated_2020q2q3 <- dat_model |> 
  model(arima = ARIMA(formula = growth_rate_to_interp, ic = "bic")) |> 
  interpolate(dat_model) |> 
  filter(date %in% yearquarter(c("2020Q2", "2020Q3"))) 

# Original data combined with interpolated values
dat_model_2 <- dat_model |> 
  left_join(y = interpolated_2020q2q3, 
            by = "date") |>
  mutate(growth_rate_to_model = ifelse(quarter %in% c("2020Q2", "2020Q3"), 
                                       growth_rate_to_interp.y, 
                                       growth_rate)) |> 
  select(-growth_rate_to_interp.x, -growth_rate_to_interp.y)

# Forecasting model  
model <- dat_model_2 |> 
  model(
    arima = ARIMA(formula = growth_rate_to_model ~ 1 + 
                    pdq(p = 0:8, d = 0:2, q = 0:8) + 
                    PDQ(P = 0, D = 0, Q = 0), 
                  ic = "bic")
  )

# Mean forecast
forecast_mean <- model |> 
  forecast(h = forecast_periods) |> 
  as_tibble() |> 
  select(date, .mean)

# Uncertainty simulations (bootstrapped)
forecast_uncertainty_simulations <- model |> 
  generate(h = forecast_periods, 
           times = forecast_uncertainty_reps, 
           bootstrap = TRUE) |> 
  as_tibble()

# Forecast uncertainty intervals as percentiles of simulated values
forecast_uncertainty_intervals <- forecast_uncertainty_simulations |> 
  group_by(date) |>
  summarise(conf_lower = quibble(x = .sim, 
                                 q = c(0.025, 0.1, 0.175)), 
            conf_upper = quibble(x = .sim, 
                                 q = c(0.975, 0.9, 0.825))) |>
  ungroup()

# *****************************************************************************


# *****************************************************************************
# Visualise ----

# Quarterly growth rate forecasts
chart_forecasts_dat <- bind_rows(
  # Actuals
  dat_model_2 |> 
    select(date, growth_rate) |> 
    filter(year(date) > 2017) |> 
    as_tibble() |> 
    mutate(type = "actual"), 
  
  # Mean forecasts
  forecast_mean |> 
    rename(growth_rate = .mean) |> 
    mutate(type = "forecast")
) |> 
  arrange(date) |> 
  mutate(sign = ifelse(growth_rate < 0, "neg", "pos")) |> 
  mutate(vjust = ifelse(growth_rate < 0, 1.5, -0.5))

chart_forecasts <- chart_forecasts_dat |> 
  ggplot(mapping = aes(x = date, 
                       y = growth_rate, 
                       colour = sign)) + 
  geom_hline(yintercept = 0, size = 0.25) + 
  geom_point(shape = 95, 
             size = 3.75, 
             stroke = 0) + 
  geom_text(mapping = aes(vjust = vjust, 
                          label = comma(x = 100 * growth_rate, 
                                        accuracy = 0.1))) + 
  geom_point(shape = 95, 
             size = 3.75, 
             stroke = 0, 
             colour = "black", 
             data = forecast_uncertainty_intervals, 
             mapping = aes(y = conf_lower$x)) + 
  geom_point(shape = 95, 
             size = 3.75, 
             stroke = 0, 
             colour = "black", 
             data = forecast_uncertainty_intervals, 
             mapping = aes(y = conf_upper$x))

# Uncertainty simulations


# *****************************************************************************