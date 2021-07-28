# Forecasts of New Zealand's quarterly seasonally adjusted real GDP growth rate

# Created by aaron@schiff.nz
# https://github.com/aaronschiff/honest-economic-forecasts 

# Data updated to: 2021 Q1

# TODO: Tidy up chart formatting
# TODO: Save forecasts (mean & intervals) to file

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
source(here("src/constants.R"))

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
  rename(gdp = amount) |> 
  mutate(growth_rate = gdp / lag(gdp) - 1) |> 
  as_tsibble(index = date, regular = TRUE)

# *****************************************************************************


# *****************************************************************************
# Forecast ----
# Values for 2020Q2 is replaced with modelled (interpolated) value to
# prevent covid lockdown shock from affecting forecasts too much. 

# Data for 2020Q2 interpolation
dat_interp_base <- dat |> 
  mutate(gdp_to_interp = ifelse(quarter %in% c("2020Q2"), NA_real_, gdp))

# Interpolated value
interpolated_2020q2 <- dat_interp_base |> 
  model(arima = ARIMA(formula = gdp_to_interp, ic = "bic")) |> 
  interpolate(dat_interp_base) |> 
  filter(date == yearquarter("2020Q2")) |> 
  pull(gdp_to_interp)

# Original data combined with interpolated value for 2020Q2
# gdp and gdp_to_model are the same except for in 2020Q2
dat_model <- dat |> 
  mutate(gdp_to_model = ifelse(quarter == "2020Q2", interpolated_2020q2, gdp))

# Forecasting model  
model_lambda <- dat_model |>
  features(.var = gdp_to_model, features = guerrero) |>
  pull(lambda_guerrero) 

model <- dat_model |> 
  model(
    arima = ARIMA(formula = box_cox(x = gdp_to_model,
                                    lambda = model_lambda), 
                  ic = "bic")
  )

# Last actual value (for calculating forecast growth rates)
last_actual <- dat |> 
  slice_max(order_by = date, n = 1) |> 
  select(date, gdp) |> 
  as_tibble()

# Mean forecast level and growth rate
forecast_mean <- model |> 
  forecast(h = forecast_periods) |> 
  as_tibble() |> 
  select(date, .mean) |>
  bind_rows(last_actual |> rename(.mean = gdp)) |> 
  arrange(date) |> 
  mutate(.mean_growth_rate = .mean / lag(.mean) - 1) |> 
  filter(!is.na(.mean_growth_rate))

# Uncertainty simulations for levels and growth rates
forecast_uncertainty_sims <- model |> 
  generate(h = forecast_periods, 
           times = forecast_uncertainty_reps, 
           bootstrap = FALSE) |> 
  as_tibble() |> 
  select(-.model) |> 
  bind_rows(
    tibble(.rep = as.character(1:forecast_uncertainty_reps), 
           last_actual |> rename(.sim = gdp))
  ) |> 
  arrange(.rep, date) |> 
  group_by(.rep) |> 
  mutate(.sim_growth_rate = .sim / lag(.sim) - 1) |> 
  ungroup() |> 
  filter(!is.na(.sim_growth_rate))

# Forecast uncertainty intervals of growth rates as percentiles of 
# simulated growth rates
forecast_uncertainty_intervals <- full_join(
  # 95% interval lower limit
  x = forecast_uncertainty_sims |> 
    group_by(date) |>
    summarise(conf_lower = quantile(x = .sim_growth_rate, probs = 0.025)), 
  
  # 95% interval upper limit
  y = forecast_uncertainty_sims |> 
    group_by(date) |>
    summarise(conf_upper = quantile(x = .sim_growth_rate, probs = 0.975)), 
  
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
  mutate(vjust = label_vjust(growth_rate))

# Create data for for visualisation - prepend last actual value of
# growth rate
last_actual_growth_rate <- dat |> 
  as_tibble() |> 
  slice_max(order_by = date, n = 1) |> 
  select(date, growth_rate)

vis_uncertainty <- bind_rows(
  forecast_uncertainty_sims, 
  tibble(.rep = as.character(1:forecast_uncertainty_reps), 
         last_actual_growth_rate |> rename(.sim_growth_rate = growth_rate))
) |> 
  arrange(.rep, date)

vis_forecast_mean <- bind_rows(
  forecast_mean, 
  last_actual_growth_rate |> rename(.mean_growth_rate = growth_rate)
) |> 
  arrange(date) |> 
  mutate(vjust = label_vjust(.mean_growth_rate))

vis_intervals <- bind_rows(
  forecast_uncertainty_intervals |> 
    mutate(type = "forecast"), 
  last_actual_growth_rate |> 
    mutate(type = "actual")
) |> 
  mutate(conf_lower = ifelse(is.na(conf_lower), growth_rate, conf_lower), 
         conf_upper = ifelse(is.na(conf_upper), growth_rate, conf_upper)) |> 
  select(-growth_rate) |> 
  arrange(date) |> 
  pivot_longer(cols = c("conf_lower", "conf_upper"), 
               names_to = "limit", 
               values_to = "value")

# Create visualisation
chart_forecasts <- ggplot() +
  # Uncertainty simulations
  geom_line(data = vis_uncertainty, 
            mapping = aes(x = date, 
                          y = .sim_growth_rate, 
                          group = .rep), 
            size = 0.1, 
            alpha = 0.1) + 
  
  # Actuals
  geom_line(data = vis_actuals, 
            mapping = aes(x = date, 
                          y = growth_rate), 
            colour = "red") + 
  geom_text_custom(data = vis_actuals, 
                   mapping = aes(x = date, 
                                 y = growth_rate, 
                                 label = comma(x = 100 * growth_rate, 
                                               accuracy = 0.1), 
                                 vjust = vjust), 
                   colour = "red") + 
  
  # 95% uncertainty interval
  geom_line(data = vis_intervals, 
            mapping = aes(x = date, 
                          y = value, 
                          group = limit), 
            colour = "blue", 
            linetype = "dashed") + 
  geom_text_custom(data = vis_intervals |> 
                     filter(type == "forecast", limit == "conf_upper"), 
                   mapping = aes(x = date, 
                                 y = value, 
                                 label = comma(x = 100 * value, 
                                               accuracy = 0.1), 
                                 vjust = vjust_up), 
                   colour = "blue") + 
  geom_text_custom(data = vis_intervals |> 
                     filter(type == "forecast", limit == "conf_lower"), 
                   mapping = aes(x = date, 
                                 y = value, 
                                 label = comma(x = 100 * value, 
                                               accuracy = 0.1), 
                                 vjust = vjust_down), 
                   colour = "blue") + 
  
  # Mean forecast
  geom_line(data = vis_forecast_mean, 
            mapping = aes(x = date, 
                          y = .mean_growth_rate), 
            colour = "blue") + 
  geom_text_custom(data = vis_forecast_mean |> filter(!is.na(.mean)), 
                   mapping = aes(x = date, 
                                 y = .mean_growth_rate, 
                                 label = comma(x = 100 * .mean_growth_rate, 
                                               accuracy = 0.1), 
                                 vjust = vjust), 
                   colour = "blue") + 
  
  # Scales
  scale_y_continuous(labels = percent_format(accuracy = 1))

# *****************************************************************************