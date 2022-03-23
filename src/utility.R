# Utility functions for forecasting


# *****************************************************************************
# Font setup ----

systemfonts::register_font(
  name = "National 2 Custom", 
  plain = systemfonts::system_fonts() |> filter(family == "National 2", style == "Regular") |> pull(path), 
  bold = systemfonts::system_fonts() |> filter(family == "National 2", style == "Extrabold") |> pull(path), 
  italic = systemfonts::system_fonts() |> filter(family == "National 2", style == "Regular Italic") |> pull(path), 
  bolditalic = systemfonts::system_fonts() |> filter(family == "National 2", style == "Extrabold Italic") |> pull(path), 
  features = systemfonts::font_feature(ligatures = c("discretionary", 
                                                     "standard", 
                                                     "contextual"), 
                                       numbers = c("lining", "proportional"))
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

geom_text_custom <- function(family = "National 2 Custom",
                             fontface = "bold",
                             ...) {
  t <- shadowtext::geom_shadowtext(family = family,
                                   fontface = fontface,
                                   bg.colour = "white", 
                                   bg.r = 0.08, 
                                   ...)
  return(t)
}


# *****************************************************************************