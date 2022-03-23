# Forecasts of New Zealand's seasonally-adjusted unemployment rate

# Created by aaron@schiff.nz
# https://github.com/aaronschiff/honest-economic-forecasts 

# Data updated to: 2021 Q4

# *****************************************************************************
# Setup ----

# Forecast configuration
series <- "unemployment"
latest_data <- "2021Q4"
forecast_periods <- 8
forecast_uncertainty_reps <- 5000
forecast_label_y <- 0.075

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
library(parallel)

# Conflicts
conflict_prefer(name = "filter", winner = "dplyr")
conflict_prefer(name = "lag", winner = "dplyr")

# Utility
source(here("src/utility.R"))
source(here("src/constants.R"))

# *****************************************************************************


# *****************************************************************************
# Load data ----

# Read source data and select those for forecasting
dat <- read_csv(file = here(glue("data/{series}/{latest_data}/{series}.csv")), 
                col_types = "cn", 
                skip = 1) |>
  clean_names() |>  
  rename(period = x1, unemp = hlfq_s1f3s) |> 
  filter(!is.na(unemp)) |> 
  separate(col = period, into = c("year", "quarter"), sep = "Q", 
           convert = TRUE) |>
  mutate(quarter = as.integer(quarter)) |> 
  mutate(date = yearquarter(glue("{year}Q{quarter}"))) |>
  filter(year > 1989) |> 
  select(date, year, quarter, unemp) |> 
  mutate(unemp = unemp / 100) |> 
  as_tsibble(index = date, regular = TRUE)

# *****************************************************************************


# *****************************************************************************
# Forecast ----

# Data for forecasting
dat_model <- dat

# Forecasting model
model_lambda <- dat_model |>
  features(.var = unemp, features = guerrero) |>
  pull(lambda_guerrero) 

model <- dat_model |> 
  model(
    arima = ARIMA(formula = box_cox(x = unemp,
                                    lambda = model_lambda),
                  ic = "bic"), 
    ets = ETS(formula = box_cox(x = unemp,
                                lambda = model_lambda), 
              ic = "bic")
  ) |> 
  mutate(blend = (arima + ets) / 2)

# Mean forecast
forecast_mean <- model |> 
  forecast(h = forecast_periods) |> 
  as_tibble() |>
  filter(.model == "blend") |> 
  select(date, .mean)

# Uncertainty simulations
forecast_uncertainty_sims <- model |> 
  generate(h = forecast_periods, 
           times = forecast_uncertainty_reps, 
           bootstrap = FALSE) |> 
  as_tibble() |> 
  filter(.model == "blend") |> 
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
  mutate(vjust = label_vjust(unemp))

# Create data for for visualisation - prepend last actual value of
# unemployment rate
last_actual_unempl_rate <- dat |> 
  as_tibble() |> 
  slice_max(order_by = date, n = 1) |> 
  select(date, unemp)

vis_uncertainty <- bind_rows(
  forecast_uncertainty_sims, 
  tibble(.rep = as.character(1:forecast_uncertainty_reps), 
         last_actual_unempl_rate |> rename(.sim = unemp))
) |> 
  arrange(.rep, date)

vis_forecast_mean <- bind_rows(
  forecast_mean |> 
    select(date, .mean) |> 
    mutate(type = "forecast"), 
  last_actual_unempl_rate |> 
    rename(.mean = unemp) |>
    mutate(type = "actual")
) |> 
  arrange(date) |> 
  mutate(vjust = label_vjust(.mean))

vis_intervals <- bind_rows(
  forecast_uncertainty_intervals |> 
    mutate(type = "forecast"), 
  last_actual_unempl_rate |> 
    mutate(type = "actual")
) |> 
  mutate(conf_lower = ifelse(is.na(conf_lower), unemp, conf_lower), 
         conf_upper = ifelse(is.na(conf_upper), unemp, conf_upper), 
         central_lower = ifelse(is.na(central_lower), unemp, central_lower), 
         central_upper = ifelse(is.na(central_upper), unemp, central_upper)) |> 
  select(-unemp) |> 
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
           y = forecast_label_y, 
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
                          y = unemp), 
            colour = colour_actuals, 
            size = linesize_actuals) + 
  
  # Actuals labels
  geom_text_custom(data = vis_actuals, 
                   mapping = aes(x = date, 
                                 y = unemp, 
                                 label = comma(x = 100 * unemp, 
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
                           y = unemp), 
             colour = colour_actuals, 
             size = point_size) + 
  
  # Scales
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 0.08),
                     breaks = seq(0, 0.08, 0.01)) +
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

# Forecast uncertainty intervals
write_csv(x = forecast_uncertainty_intervals, 
          file = here(glue("forecasts/{series}/{latest_data}/{series}_forecast_uncertainty_intervals.csv")))

# *****************************************************************************


# *****************************************************************************
# Validation ----
# Validate forecasts by using data up to end of 2017 to forecast for 2018 &
# 2019, then compare to actuals

select_lambda <- function(x) {
  f <- features(.tbl = as_tsibble(x), .var = unemp, features = guerrero)
  l <- f$lambda_guerrero
  return(l)
}

apply_bc <- function(d, l) {
  dbc <- d |> 
    mutate(unemp = box_cox(x = unemp, lambda = l))
  return(dbc)
}

model_unempl <- function(d) {
  m <- d |> 
    model(
      arima = ARIMA(formula = unemp, ic = "bic"), 
    ) 
  return(m$arima)
}

folds_for_window <- function(d, w, h) {
  f <- d |> 
    tail(10 + h + w - 1) |> 
    slide_tsibble(.size = w) |> 
    as_tibble()
  return(f)
}

dat_model_validation <- tibble(
  w = seq(20L, 80L, 4L)
) |> 
  rowwise() |> 
  mutate(dat_fold = list(folds_for_window(d = dat, 
                                          w = w, 
                                          h = 8L))) |> 
  unnest(cols = dat_fold) |> 
  ungroup() |> 
  arrange(w, .id, date)

model_validation <- dat_model_validation |> 
  nest_by(w, .id) |> 
  mutate(data = list(as_tsibble(data, index = date))) |> 
  mutate(l = select_lambda(data)) |> 
  mutate(data_bc = list(apply_bc(d = data, l = l))) |> 
  mutate(arima_bc = model_unempl(d = data_bc), 
         arima = model_unempl(d = data))

forecast_mean_validation <- bind_rows(
  model_validation |>
    mutate(fc_arima_bc = list(forecast(object = arima_bc, h = 8L))) |> 
    select(w, .id, l, fc_arima_bc) |> 
    unnest(cols = fc_arima_bc) |> 
    ungroup() |> 
    mutate(.mean = inv_box_cox(x = .mean, lambda = l)) |> 
    mutate(bc = "yes"), 
  model_validation |>
    mutate(fc_arima = list(forecast(object = arima, h = 8L))) |> 
    select(w, .id, l, fc_arima) |> 
    unnest(cols = fc_arima) |> 
    ungroup() |> 
    mutate(bc = "no")
) |> 
  select(bc, w, .id, date, .mean, l) |> 
  arrange(bc, w, .id, date) |> 
  group_by(bc, w, .id) |> 
  mutate(h = row_number()) |> 
  ungroup() |> 
  select(-l) 

sim_for_model <- function(i, d, h, times, bootstrap, bc) {
  if (bc) {
    y <- generate(x = d[[i, "arima_bc"]][[1]], 
                  h = h, 
                  times = times, 
                  bootstrap = bootstrap)
  } else {
    y <- generate(x = d[[i, "arima"]][[1]], 
                  h = h, 
                  times = times, 
                  bootstrap = bootstrap)
  }
  y <- y |> 
    mutate(w = d[[i, "w"]][[1]], 
           .id = d[[i, ".id"]][[1]], 
           l = d[[i, "l"]][[1]], 
           bc = bc)
  return(as_tibble(y))
}

z <- mclapply(X = 1:nrow(model_validation), 
              FUN = sim_for_model, 
              d = model_validation, 
              h = 8L, 
              times = 1000, 
              bootstrap = FALSE, 
              bc = FALSE, 
              mc.cores = 8) |> 
  bind_rows()

forecast_uncertainty_validation <- model_validation |> 
  mutate(us = list(generate(x = arima, 
                            h = 8L, 
                            times = 1000, 
                            bootstrap = FALSE)), 
         us_bc = list(generate(x = arima_bc, 
                               h = 8L, 
                               times = 1000, 
                               bootstrap = FALSE))) 

forecast_uncertainty_validation_ci <- bind_rows(
  forecast_uncertainty_validation |> 
    mutate(us_bc = list(as_tibble(us_bc))) |> 
    select(w, .id, l, us_bc) |> 
    unnest(cols = us_bc) |> 
    ungroup() |> 
    mutate(.sim = inv_box_cox(x = .sim, lambda = l)) |> 
    mutate(bc = "yes"), 
  forecast_uncertainty_validation |> 
    mutate(us = list(as_tibble(us))) |> 
    select(w, .id, l, us) |> 
    unnest(cols = us) |> 
    ungroup() |> 
    mutate(bc = "no")
) |> 
  group_by(bc, w, .id, date) |> 
  summarise(conf_lower = quantile(.sim, probs = 0.05), 
            conf_upper = quantile(.sim, probs = 0.95), 
            central_lower = quantile(.sim, probs = 0.25), 
            central_upper = quantile(.sim, probs = 0.75)) |> 
  ungroup() 

dat_validation <- forecast_mean_validation |> 
  left_join(y = forecast_uncertainty_validation_ci, 
            by = c("bc", "w", ".id", "date")) 

dat_validation_summary <- dat_validation |> 
  filter(year(date) < 2022) |> 
  left_join(y = dat, by = "date") |> 
  mutate(ape = abs((unemp - .mean) / unemp)) |> 
  mutate(in_conf = ifelse((unemp < conf_upper) & (unemp > conf_lower), 1L, 0L), 
         in_central = ifelse((unemp < central_upper) & (unemp > central_lower), 1L, 0L)) |> 
  group_by(bc, w, h) |> 
  summarise(mape = mean(ape), 
            in_conf_n = sum(in_conf), 
            in_central_n = sum(in_central), 
            n = n()) |> 
  ungroup() |> 
  mutate(in_conf_pct = in_conf_n / n, 
         in_central_pct = in_central_n / n)

chart_validation_summary_mape <- dat_validation_summary |> 
  group_by(bc, w) |> 
  summarise(mean_mape = mean(mape)) |> 
  ungroup() |> 
  ggplot(mapping = aes(x = w, 
                       y = mean_mape, 
                       fill = bc)) + 
  geom_col(position = position_dodge()) 

chart_validation_summary_in_central <- dat_validation_summary |> 
  group_by(bc, w) |> 
  summarise(mean_in_central_pct = mean(in_central_pct)) |> 
  ungroup() |> 
  ggplot(mapping = aes(x = w, 
                       y = mean_in_central_pct, 
                       fill = bc)) + 
  geom_col(position = position_dodge()) 

chart_validation_summary_in_conf <- dat_validation_summary |> 
  group_by(bc, w) |> 
  summarise(mean_in_cont_pct = mean(in_conf_pct)) |> 
  ungroup() |> 
  ggplot(mapping = aes(x = w, 
                       y = mean_in_cont_pct, 
                       fill = bc)) + 
  geom_col(position = position_dodge()) 

chart_validation <- dat_validation |> 
  filter(w == 36L, bc == "no") |> 
  ggplot(mapping = aes(x = date)) + 
  geom_pointrange(mapping = aes(y = .mean, 
                                ymin = conf_lower, 
                                ymax = conf_upper), 
                  colour = "firebrick4") + 
  geom_point(mapping = aes(y = unemp), 
             colour = "cornflowerblue", 
             data = dat |> filter(year > 2014)) + 
  facet_wrap(facets = vars(.id))



# *****************************************************************************