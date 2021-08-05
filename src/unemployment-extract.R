# Extract unemployment rate data from giant Stats NZ labour market stats file

library(tidyverse)
library(here)
library(janitor)

dat <- read_csv(file = here("data/unemployment/raw/2021Q2/unemployment.csv"), 
                col_types = "ccncciccccccc") |> 
  clean_names()

unemp <- dat |> 
  filter(group == "Labour Force Status by Sex: Seasonally Adjusted") |> 
  filter(series_title_1 == "Unemployment Rate") |> 
  filter(series_title_2 == "Total Both Sexes") 

write_csv(x = unemp, 
          file = here("data/unemployment/2021Q2/unemployment.csv"))
