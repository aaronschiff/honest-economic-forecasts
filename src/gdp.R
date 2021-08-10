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

# Conflicts
conflict_prefer(name = "filter", winner = "dplyr")
conflict_prefer(name = "lag", winner = "dplyr")

# Utility
source(here("src/utility.R"))
source(here("src/constants.R"))

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
  filter(year(date) > 1989) |> 
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
  mutate(gdp_to_model = ifelse(quarter == "2020Q2", interpolated_2020q2, gdp)) |> 
  mutate(growth_rate_to_model = gdp_to_model / lag(gdp_to_model) - 1) |> 
  filter(year(date) > 1989)

# Forecasting model  
model <- dat_model |> 
  model(
    arima = ARIMA(formula = growth_rate_to_model, ic = "bic"), 
    ets = ETS(formula = growth_rate_to_model, ic = "bic")
  ) |> 
  mutate(blend = (arima + ets) / 2)

# Mean forecast level and growth rate
forecast_mean <- model |> 
  forecast(h = forecast_periods) |> 
  as_tibble() |> 
  filter(.model == "blend") |> 
  select(date, .mean) 

# Uncertainty simulations for levels and growth rates
forecast_uncertainty_sims <- model |> 
  generate(h = forecast_periods, 
           times = forecast_uncertainty_reps, 
           bootstrap = FALSE) |> 
  as_tibble() |> 
  filter(.model == "blend") |> 
  select(-.model) 

# Forecast uncertainty intervals of growth rates as percentiles of 
# simulated growth rates
forecast_uncertainty_intervals <- full_join(
  # 90% interval lower limit
  x = forecast_uncertainty_sims |> 
    group_by(date) |>
    summarise(conf_lower = quantile(x = .sim, probs = 0.025)), 
  
  # 90% interval upper limit
  y = forecast_uncertainty_sims |> 
    group_by(date) |>
    summarise(conf_upper = quantile(x = .sim, probs = 0.975)), 
  
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
         last_actual_growth_rate |> rename(.sim = growth_rate))
) |> 
  arrange(.rep, date)

vis_forecast_mean <- bind_rows(
  forecast_mean, 
  last_actual_growth_rate |> rename(.mean = growth_rate)
) |> 
  arrange(date) |> 
  mutate(vjust = label_vjust(.mean))

vis_intervals <- bind_rows(
  forecast_uncertainty_intervals |> 
    mutate(type = "forecast"), 
  last_actual_growth_rate |> 
    mutate(type = "actual")
) |> 
  mutate(conf_lower = ifelse(is.na(conf_lower), growth_rate, conf_lower), 
         conf_upper = ifelse(is.na(conf_upper), growth_rate, conf_upper), 
         central_lower = ifelse(is.na(central_lower), growth_rate, central_lower), 
         central_upper = ifelse(is.na(central_upper), growth_rate, central_upper)) |> 
  select(-growth_rate) |> 
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
  
  # Actual / forecast line
  geom_vline(xintercept = as.Date(yearquarter(latest_data)), 
             size = linesize_af, 
             colour = colour_af, 
             alpha = alpha_af, 
             linetype = linetype_af) + 
  
  # Forecast period label
  annotate(geom = "text", 
           x = as.Date(yearquarter(latest_data)), 
           y = max(vis_actuals$growth_rate), 
           hjust = -0.15, 
           label = "Forecast", 
           family = "Fira Sans Custom", 
           fontface = "bold", 
           size = 2.5, 
           colour = colour_af_label) + 
  
  # Uncertainty simulations
  geom_line(data = vis_uncertainty, 
            mapping = aes(x = date, 
                          y = .sim, 
                          group = .rep), 
            size = linesize_uncertainty, 
            colour = colour_uncertainty, 
            alpha = alpha_uncertainty) + 
  
  # Mean forecast points
  geom_point(data = vis_forecast_mean, 
             mapping = aes(x = date, 
                           y = .mean), 
             colour = colour_forecast_mean, 
             size = point_size) + 
  
  # Uncertainty interval points
  geom_point(data = vis_intervals |> 
               filter(limit %in% c("conf_lower", "conf_upper")), 
             mapping = aes(x = date, 
                           y = value), 
             colour = colour_forecast_intervals, 
             size = point_size) + 
  
  # Actuals line
  geom_line(data = vis_actuals, 
            mapping = aes(x = date, 
                          y = growth_rate), 
            colour = colour_actuals, 
            size = linesize_actuals) + 
  
  # Actuals labels
  geom_text_custom(data = vis_actuals, 
                   mapping = aes(x = date, 
                                 y = growth_rate, 
                                 label = comma(x = 100 * growth_rate, 
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
  geom_text_custom(data = vis_forecast_mean |> filter(!is.na(.mean)), 
                   mapping = aes(x = date, 
                                 y = .mean, 
                                 label = comma(x = 100 * .mean, 
                                               accuracy = 0.1), 
                                 vjust = vjust), 
                   colour = colour_forecast_mean) + 
  
  # Actual points
  geom_point(data = vis_actuals, 
             mapping = aes(x = date, 
                           y = growth_rate), 
             colour = colour_actuals, 
             size = point_size) + 
  
  # Scales
  scale_y_continuous(labels = percent_format(accuracy = 1)) + 
  scale_x_yearquarter(date_breaks = "1 year")

output_chart(chart = chart_forecasts, 
             filename = series, 
             path = here(glue("forecasts/{series}/{latest_data}")), 
             orientation = "wide", 
             xlab = "", 
             ylab = "")

# *****************************************************************************


# *****************************************************************************
# Save forecast ----

# Forecast mean
write_csv(x = forecast_mean, 
          file = here(glue("forecasts/{series}/{latest_data}/{series}_forecast_mean.csv")))

# Forecast uncertainty simulations
write_csv(x = forecast_uncertainty_sims, 
          file = here(glue("forecasts/{series}/{latest_data}/{series}_forecast_uncertainty_sims.csv")))

# Forecast uncertainty interval
write_csv(x = forecast_uncertainty_intervals, 
          file = here(glue("forecasts/{series}/{latest_data}/{series}_forecast_uncertainty_intervals.csv")))

# *****************************************************************************


# *****************************************************************************
# Validation ---- 
# Validate forecasts by using data up to end of 2017 to forecast for 2018 &
# 2019, then compare to actuals

# Data for forecasting
dat_model_validation <- dat_model |> filter(year(date) < 2018)

# Forecasting model
model_validation <- dat_model_validation |> 
  model(
    arima = ARIMA(formula = growth_rate_to_model, ic = "bic"), 
    ets = ETS(formula = growth_rate_to_model, ic = "bic"), 
  ) |> 
  mutate(blend = (arima + ets) / 2)

# Mean forecast level and growth rate
forecast_mean_validation <- model_validation |> 
  forecast(h = forecast_periods) |> 
  as_tibble() |> 
  filter(.model == "blend") |> 
  select(date, .mean) 

# Uncertainty simulations for levels and growth rates
forecast_uncertainty_sims_validation <- model_validation |> 
  generate(h = forecast_periods, 
           times = forecast_uncertainty_reps, 
           bootstrap = FALSE) |> 
  as_tibble() |> 
  filter(.model == "blend") |> 
  select(-.model) 

# Forecast uncertainty intervals of growth rates as percentiles of 
# simulated growth rates
forecast_uncertainty_intervals_validation <- full_join(
  # 90% interval lower limit
  x = forecast_uncertainty_sims_validation |> 
    group_by(date) |>
    summarise(conf_lower = quantile(x = .sim, probs = 0.025)), 
  
  # 90% interval upper limit
  y = forecast_uncertainty_sims_validation |> 
    group_by(date) |>
    summarise(conf_upper = quantile(x = .sim, probs = 0.975)), 
  
  by = "date"
) |>
  # 50% interval lower limit
  full_join(
    y = forecast_uncertainty_sims_validation |> 
      group_by(date) |>
      summarise(central_lower = quantile(x = .sim, probs = 0.25)), 
    by = "date"
  ) |> 
  # 50% interval upper limit
  full_join(
    y = forecast_uncertainty_sims_validation |> 
      group_by(date) |>
      summarise(central_upper = quantile(x = .sim, probs = 0.75)), 
    by = "date"
  ) |> 
  pivot_longer(cols = -date, names_to = "limit", values_to = "value")

# Create visualisation
chart_validation <- ggplot() +
  # Zero line
  geom_hline(yintercept = 0, 
             size = linesize_zeroline, 
             colour = colour_zeroline) +
  
  # Uncertainty simulations
  geom_line(data = forecast_uncertainty_sims_validation, 
            mapping = aes(x = date, 
                          y = .sim, 
                          group = .rep), 
            size = linesize_uncertainty, 
            colour = colour_uncertainty, 
            alpha = alpha_uncertainty) + 
  
  # Mean forecast points
  geom_point(data = forecast_mean_validation, 
             mapping = aes(x = date, 
                           y = .mean), 
             colour = colour_forecast_mean, 
             size = point_size) + 
  
  # 90% uncertainty interval points
  geom_point(data = forecast_uncertainty_intervals_validation |> 
               filter(limit %in% c("conf_lower", "conf_upper")), 
             mapping = aes(x = date, 
                           y = value), 
             colour = colour_forecast_intervals, 
             size = point_size) + 
  
  # Actuals line
  geom_line(data = dat |> filter(date %in% forecast_mean_validation$date), 
            mapping = aes(x = date, 
                          y = growth_rate), 
            colour = colour_actuals, 
            size = linesize_actuals) + 
  
  # 90% uncertainty interval lines
  geom_line(data = forecast_uncertainty_intervals_validation |> 
              filter(limit %in% c("conf_lower", "conf_upper")), 
            mapping = aes(x = date, 
                          y = value, 
                          group = limit), 
            colour = colour_forecast_intervals, 
            size = linesize_forecast_intervals, 
            linetype = linetype_forecast_intervals) + 
  
  # Central (50%) uncertainty lines
  geom_line(data = forecast_uncertainty_intervals_validation |> 
              filter(limit %in% c("central_lower", "central_upper")), 
            mapping = aes(x = date, 
                          y = value, 
                          group = limit), 
            colour = colour_forecast_central, 
            size = linesize_forecast_central, 
            linetype = linetype_forecast_central) + 
  
  # Mean forecast line
  geom_line(data = forecast_mean_validation, 
            mapping = aes(x = date, 
                          y = .mean), 
            colour = colour_forecast_mean, 
            size = linesize_forecast_mean) + 
  
  # Actual points
  geom_point(data = dat |> filter(date %in% forecast_mean_validation$date), 
             mapping = aes(x = date, 
                           y = growth_rate), 
             colour = colour_actuals, 
             size = point_size) + 
  
  # Scales
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(-0.02, 0.03),
                     breaks = seq(-0.02, 0.03, 0.01)) +
  scale_x_yearquarter(date_breaks = "3 months")

output_chart(chart = chart_validation, 
             filename = glue("{series}_validation"), 
             path = here(glue("forecasts/{series}")), 
             orientation = "wide", 
             xlab = "", 
             ylab = "")

# *****************************************************************************
