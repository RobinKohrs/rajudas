library(tidyverse)
library(here)
library(glue)
library(sf)
library(rajudas)
library(jsonlite)

a = read_sf("~/geodata/österreich/bundeslaender/VGD_Oesterreich_gst_20221002/bundeslaender.gpkg")
bls_au = a %>%
  group_by(BL) %>%
  summarise(bl_kz = first(BL_KZ))

usethis::use_data(bls_au)
