library(tidyverse)
library(here)
library(sf)
library(rajudas)

gems_2023 = rajudas::oe_gem_data() %>% st_transform(31287)

major_cities = c("10101|20101|30201|40101|50101|60101|70101|80207|^9")

austria_capitals = rajudas::oe_gem_data() %>%
  filter(
    str_detect(g_id, major_cities)
  ) %>%
  mutate(
    name = case_when(
      str_detect(g_id, "^9") ~ "Wien",
      .default = g_name
    )
  ) %>%
  group_by(name) %>%
  summarise() %>%
  st_cast("POLYGON")


# this is the name it will have
usethis::use_data(austria_capitals, overwrite = T)

