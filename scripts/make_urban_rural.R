library(tidyverse)
library(here)
library(glue)
library(sf)
library(rajudas)
library(jsonlite)


raw = fromJSON("https://www.statistik.at/gs-atlas/ATLAS_IMAP_WFS/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=ATLAS_IMAP_WFS:DATA_GEM_GDB_DDT_V_PG&outputFormat=application%2Fjson&viewparams=DDT:mv_t0281_topo_stadt_land_2022;YEAR:2022-01-01;V1:CODE_STAT;V2:CODE_STAT;GEN:100;WIENFLAG:1;")

data_urban_rural = raw$features$properties %>% janitor::clean_names()

data_clean = data_urban_rural %>%
  select(gkz=id, name, wert=v1)

map(1:23, function(g){
  id_wien = str_pad(g, width = 2, pad=0)
  data.frame(
    gkz = glue("9{id_wien}01"),
    name = "",
    wert = as.character(101)
  )
}) %>% bind_rows() -> wien_urban_rural

data_urban_rural = data_clean %>%
  filter(
    gkz != 90001
  ) %>% bind_rows(wien_urban_rural)



data_urban_rural %>%
  mutate(class = wert) %>%
  mutate(
    classname = case_when(
      class == 101 ~ "Urbane Großzentren",
      class == 102 ~ "Urbane Mittelzentren",
      class == 103 ~ "Urbane Kleinzentren",
      class == 210 ~ "Regionale Zentren, zentral",
      class == 220 ~ "Regionale Zentren, intermediär",
      class == 310 ~ "Ländlicher Raum im Umland von Zentren, zentral",
      class == 320 ~ "Ländlicher Raum im Umland von Zentren, intermediär",
      class == 330 ~ "Ländlicher Raum im Umland von Zentren, periphär",
      class == 410 ~ "Ländlicher Raum, zentral",
      class == 420 ~ "Ländlicher Raum, intermediär",
      class == 430 ~ "Ländlicher Raum, periphär",
    ),
     color = case_when(
      class == 101 ~ "#be566a",
      class == 102 ~ "#e36766",
      class == 103 ~ "#eb887e",
      class == 210 ~ "#f8aa4b",
      class == 220 ~ "#fcc77a",
      class == 310 ~ "#fff333",
      class == 320 ~ "#eeec7a",
      class == 330 ~ "#eeec7a",
      class == 410 ~ "#bed486",
      class == 420 ~ "#77ba6b",
      class == 430 ~ "#6f9f6b"
    )
  ) %>% rename(class_ur = class, classname_ur = classname)  -> data_urban_rural


# use in package ----------------------------------------------------------
usethis::use_data(data_urban_rural, overwrite = T)

