# Utility functions for forecasting

# *****************************************************************************
# Quantiles in a tibble ----
quibble <- function(x, q) {
  tibble(x = quantile(x, q), q = q)
}

# *****************************************************************************

