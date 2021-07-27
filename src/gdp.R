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

# Read GDP dat and select those for forecasting
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
# prevent covid lockdown shock from affecting forecasts too much. 

# Data for 2020Q2 and Q3 interpolation
dat_interp_base <- dat |> 
  mutate(growth_rate_to_interp = ifelse(quarter %in% c("2020Q2", "2020Q3"), 
                                        NA_real_, 
                                        growth_rate))

# Interpolated values
interpolated_2020q2q3 <- dat_interp_base |> 
  model(arima = ARIMA(formula = growth_rate_to_interp, ic = "bic")) |> 
  interpolate(dat_interp_base) |> 
  filter(date %in% yearquarter(c("2020Q2", "2020Q3"))) 

# Original data combined with interpolated values
# growth_rate and growth_rate_to_model are the same except for in 2020Q2 and Q3
dat_model <- dat_interp_base |> 
  left_join(y = interpolated_2020q2q3, 
            by = "date") |>
  mutate(growth_rate_to_model = ifelse(quarter %in% c("2020Q2", "2020Q3"), 
                                       growth_rate_to_interp.y, 
                                       growth_rate)) |> 
  select(-growth_rate_to_interp.x, -growth_rate_to_interp.y)

# Forecasting model  
model <- dat_model |> 
  model(
    arima = ARIMA(formula = growth_rate_to_model, ic = "bic")
  )

# Mean forecast
forecast_mean <- model |> 
  forecast(h = forecast_periods) |> 
  as_tibble() |> 
  select(date, .mean)

# Uncertainty simulations (bootstrapped)
forecast_uncertainty_sims <- model |> 
  generate(h = forecast_periods, 
           times = forecast_uncertainty_reps, 
           bootstrap = FALSE) |> 
  as_tibble() |> 
  select(-.model)

# Forecast uncertainty intervals as percentiles of simulated values
forecast_uncertainty_intervals <- forecast_uncertainty_sims |> 
  group_by(date) |>
  summarise(conf_lower = quibble(x = .sim, 
                                 q = c(0.025, 0.1, 0.175)), 
            conf_upper = quibble(x = .sim, 
                                 q = c(0.975, 0.9, 0.825))) |>
  ungroup()

# *****************************************************************************


# *****************************************************************************
# Visualise ----

# Uncertainty simulations for visualisation - prepend last actual value
last_actual <- dat_model |> 
  as_tibble() |> 
  slice_max(order_by = date, n = 1) |> 
  select(date, .sim = growth_rate)

vis_uncertainty <- bind_rows(
  forecast_uncertainty_sims, 
  tibble(.rep = as.character(1:forecast_uncertainty_reps), 
         last_actual)
) |> 
  arrange(.rep, date)

chart_forecasts <- ggplot() +
  # Uncertainty simulations
  geom_line(data = vis_uncertainty, 
            mapping = aes(x = date, 
                          y = .sim, 
                          group = .rep), 
            size = 0.1, 
            alpha = 0.25)



# *****************************************************************************