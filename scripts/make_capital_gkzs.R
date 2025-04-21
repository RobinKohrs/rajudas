library(tidyverse)
library(here)
library(glue)
library(sf)
library(jsonlite)
devtools::load_all()


# gemeinden ---------------------------------------------------------------
geo_gems = rajudas::oe_gem_data(2024, wien_union = T, force = T)


landeshauptstaedte  = c(
  "Wien",
  "Bregenz",
  "Linz",
  "Klagenfurt am Wörthersee",
  "Eisenstadt",
  "Innsbruck",
  "Salzburg",
  "St. Pölten",
  "Graz"
)

lhs = geo_gems %>%
  filter(g_name %in% landeshauptstaedte)


# make csv ----------------------------------------------------------------
landeshauptstaedte_gkz = lhs %>% pull(g_id)
usethis::use_data(landeshauptstaedte_gkz)


# make geodata ------------------------------------------------------------
landeshauptstaedte_geo = lhs
usethis::use_data(landeshauptstaedte_geo)

