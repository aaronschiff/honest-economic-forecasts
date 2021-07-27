# Utility functions for forecasting


# *****************************************************************************
# Constants ----

vjust_up <- -0.3
vjust_down <- 1.3

# *****************************************************************************


# *****************************************************************************
# Font setup ----

systemfonts::register_variant(
  name = "Fira Sans Custom",
  family = "Fira Sans",
  weight = c("medium", "bold"),
  features = systemfonts::font_feature(ligatures = c("standard",
                                                     "contextual"))
)

# *****************************************************************************


# *****************************************************************************
# Quantiles in a tibble ----

quibble <- function(x, q) {
  tibble(x = quantile(x, q), q = q)
}

# *****************************************************************************


# *****************************************************************************
# Positions for chart labels ----

label_vjust = function(x) {
  y <- tibble(x = x) |> 
    mutate(slope_left = x - dplyr::lag(x), 
           slope_right = dplyr::lead(x) - x) |> 
    mutate(vjust = case_when(
      is.na(slope_left) & slope_right <= 0 ~ vjust_up, 
      is.na(slope_left) & slope_right > 0 ~ vjust_down, 
      slope_left <= 0 & slope_right <= 0 ~ vjust_up, 
      slope_left <= 0 & slope_right > 0 ~ vjust_down, 
      slope_left > 0 & slope_right > 0 ~ vjust_down, 
      slope_left > 0 & slope_right <= 0 ~ vjust_up, 
      slope_left <= 0 & is.na(slope_right) ~ vjust_down, 
      slope_left > 0 & is.na(slope_right) ~ vjust_up
    ))
  return(y$vjust)
}

# *****************************************************************************


# *****************************************************************************
# Custom geom_text ----

geom_text_custom <- function(family = "Fira Sans Custom",
                             rel_size = 1,
                             abs_size = 2.5,
                             fontface = "bold",
                             ...) {
  t <- ggplot2::geom_text(family = family,
                          size = rel_size * abs_size,
                          fontface = fontface,
                          ...)
  return(t)
}


# *****************************************************************************