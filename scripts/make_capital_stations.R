library(tidyverse)
library(here)
library(glue)
library(sf)
library(jsonlite)
devtools::load_all()


# all stations ------------------------------------------------------------
all_stations = rajudas::geosphere_get_stations() %>% st_transform(31287)

# geodata of capitals -----------------------------------------------------
geo_lhs = rajudas::landeshauptstaedte_geo %>% st_transform(31287)


# stations in the 9 citues ------------------------------------------------
stations_in_lhs = all_stations[geo_lhs, ]


# active stations in lhs --------------------------------------------------
stations_in_lhs_active =  stations_in_lhs %>%
  filter(is_active) %>%
  filter(valid_to > Sys.Date())

# longest running stations per city
stations_in_lhs_active %>%
  group_by(state) %>%
  filter(valid_from == min(valid_from)) -> geosphere_stations_in_lhs


# save to package ---------------------------------------------------------
usethis::use_data(geosphere_stations_in_lhs, overwrite = T)


