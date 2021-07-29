# Forecasts of New Zealand's annual CPI inflation rate

# Created by aaron@schiff.nz
# https://github.com/aaronschiff/honest-economic-forecasts 

# Data updated to: 2021 Q2

# *****************************************************************************
# Setup ----

# Forecast configuration
series <- "inflation"
latest_data <- "2021Q2"
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
source(here("src/constants.R"))

# Conflicts
conflict_prefer(name = "filter", winner = "dplyr")
conflict_prefer(name = "lag", winner = "dplyr")

# *****************************************************************************


# *****************************************************************************
# Load data ----

# Read GDP dat and select those for forecasting
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

# *****************************************************************************


# *****************************************************************************
# Forecast ----

# Data for forecasting
dat_model <- dat

# Forecasting model
model <- dat_model |> 
  model(
    arima = ARIMA(formula = inflation_rate, 
                  ic = "bic")
  )

# Mean forecast
forecast_mean <- model |> 
  forecast(h = forecast_periods) |> 
  as_tibble() |>
  select(date, .mean)

# Uncertainty simulations
forecast_uncertainty_sims <- model |> 
  generate(h = forecast_periods, 
           times = forecast_uncertainty_reps, 
           bootstrap = FALSE) |> 
  as_tibble() |> 
  select(-.model)

# Forecast uncertainty intervals of growth rates as percentiles of 
# simulated inflation rates
forecast_uncertainty_intervals <- full_join(
  # 90% interval lower limit
  x = forecast_uncertainty_sims |> 
    group_by(date) |>
    summarise(conf_lower = quantile(x = .sim, probs = 0.05)), 
  
  # 90% interval upper limit
  y = forecast_uncertainty_sims |> 
    group_by(date) |>
    summarise(conf_upper = quantile(x = .sim, probs = 0.95)), 
  
  by = "date"
) |> 
  # 50% interval lower limit
  full_join(
    y = forecast_uncertainty_sims |> 
      group_by(date) |>
      summarise(central_lower = quantile(x = .sim, probs = 0.25)), 
    by = "date"
  ) |> 
  # 50% interval upper limit
  full_join(
    y = forecast_uncertainty_sims |> 
      group_by(date) |>
      summarise(central_upper = quantile(x = .sim, probs = 0.75)), 
    by = "date"
  )

# *****************************************************************************


# *****************************************************************************
# Visualise ----

# Actuals to show on the chart: twice as many points as in the forecast
vis_actuals <- dat |> 
  as_tibble() |>
  slice_max(order_by = date, n = 2 * forecast_periods) |> 
  arrange(date) |> 
  mutate(vjust = label_vjust(inflation_rate))

# Create data for for visualisation - prepend last actual value of
# inflation rate
last_actual_inflation_rate <- dat |> 
  as_tibble() |> 
  slice_max(order_by = date, n = 1) |> 
  select(date, inflation_rate)

vis_uncertainty <- bind_rows(
  forecast_uncertainty_sims, 
  tibble(.rep = as.character(1:forecast_uncertainty_reps), 
         last_actual_inflation_rate |> rename(.sim = inflation_rate))
) |> 
  arrange(.rep, date)

vis_forecast_mean <- bind_rows(
  forecast_mean |> 
    select(date, .mean) |> 
    mutate(type = "forecast"), 
  last_actual_inflation_rate |> 
    rename(.mean = inflation_rate) |>
    mutate(type = "actual")
) |> 
  arrange(date) |> 
  mutate(vjust = label_vjust(.mean))

vis_intervals <- bind_rows(
  forecast_uncertainty_intervals |> 
    mutate(type = "forecast"), 
  last_actual_inflation_rate |> 
    mutate(type = "actual")
) |> 
  mutate(conf_lower = ifelse(is.na(conf_lower), inflation_rate, conf_lower), 
         conf_upper = ifelse(is.na(conf_upper), inflation_rate, conf_upper), 
         central_lower = ifelse(is.na(central_lower), inflation_rate, central_lower), 
         central_upper = ifelse(is.na(central_upper), inflation_rate, central_upper)) |> 
  select(-inflation_rate) |> 
  arrange(date) |> 
  pivot_longer(cols = c("conf_lower", "conf_upper", "central_lower", "central_upper"), 
               names_to = "limit", 
               values_to = "value")

# Create visualisation
chart_forecasts <- ggplot() +
  # Zero line
  geom_hline(yintercept = 0, 
             size = linesize_zeroline, 
             colour = colour_zeroline) + 
  
  # Uncertainty simulations
  geom_line(data = vis_uncertainty, 
            mapping = aes(x = date, 
                          y = .sim, 
                          group = .rep), 
            size = linesize_uncertainty, 
            alpha = alpha_uncertainty) + 
  
  # Mean forecast points
  geom_point(data = vis_forecast_mean, 
             mapping = aes(x = date, 
                           y = .mean), 
             colour = colour_forecast_mean, 
             size = point_size) + 
  
  # 90% uncertainty interval points
  geom_point(data = vis_intervals |> 
               filter(limit %in% c("conf_lower", "conf_upper")), 
             mapping = aes(x = date, 
                           y = value), 
             colour = colour_forecast_intervals, 
             size = point_size) + 
  
  # Actuals line
  geom_line(data = vis_actuals, 
            mapping = aes(x = date, 
                          y = inflation_rate), 
            colour = colour_actuals, 
            size = linesize_actuals) + 
  
  # Actuals labels
  geom_text_custom(data = vis_actuals, 
                   mapping = aes(x = date, 
                                 y = inflation_rate, 
                                 label = comma(x = 100 * inflation_rate, 
                                               accuracy = 0.1), 
                                 vjust = vjust), 
                   colour = colour_actuals) + 
  
  # 90% uncertainty interval lines
  geom_line(data = vis_intervals |> 
              filter(limit %in% c("conf_lower", "conf_upper")), 
            mapping = aes(x = date, 
                          y = value, 
                          group = limit), 
            colour = colour_forecast_intervals, 
            size = linesize_forecast_intervals, 
            linetype = linetype_forecast_intervals) + 
  
  # Central (50%) uncertainty lines
  geom_line(data = vis_intervals |> 
              filter(limit %in% c("central_lower", "central_upper")), 
            mapping = aes(x = date, 
                          y = value, 
                          group = limit), 
            colour = colour_forecast_central, 
            size = linesize_forecast_central, 
            linetype = linetype_forecast_central) + 
  
  # 90% uncertainty interval labels
  geom_text_custom(data = vis_intervals |> 
                     filter(type == "forecast", limit == "conf_upper"), 
                   mapping = aes(x = date, 
                                 y = value, 
                                 label = comma(x = 100 * value, 
                                               accuracy = 0.1), 
                                 vjust = vjust_up), 
                   colour = colour_forecast_intervals) + 
  geom_text_custom(data = vis_intervals |> 
                     filter(type == "forecast", limit == "conf_lower"), 
                   mapping = aes(x = date, 
                                 y = value, 
                                 label = comma(x = 100 * value, 
                                               accuracy = 0.1), 
                                 vjust = vjust_down), 
                   colour = colour_forecast_intervals) + 
  
  # Mean forecast line
  geom_line(data = vis_forecast_mean, 
            mapping = aes(x = date, 
                          y = .mean), 
            colour = colour_forecast_mean, 
            size = linesize_forecast_mean) + 
  
  # Mean forecast labels
  geom_text_custom(data = vis_forecast_mean |> filter(type == "forecast"), 
                   mapping = aes(x = date, 
                                 y = .mean, 
                                 label = comma(x = 100 * .mean, 
                                               accuracy = 0.1), 
                                 vjust = vjust), 
                   colour = colour_forecast_mean) + 
  
  # Actual points
  geom_point(data = vis_actuals, 
             mapping = aes(x = date, 
                           y = inflation_rate), 
             colour = colour_actuals, 
             size = point_size) + 
  
  # Scales
  scale_y_continuous(labels = percent_format(accuracy = 1), 
                     limits = c(-0.02, 0.08), 
                     breaks = seq(-0.02, 0.08, 0.01)) + 
  scale_x_yearquarter(date_breaks = "1 year")

output_chart(chart = chart_forecasts, 
             filename = series, 
             path = here(glue("forecasts/{series}/{latest_data}")), 
             orientation = "wide", 
             xlab = "", 
             ylab = "")

