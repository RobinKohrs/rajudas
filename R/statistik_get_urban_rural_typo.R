#' get the urban rural typo
#'
#' @param source either "statistik" or "eu"
#'
#' @export


statistik_get_urban_rural_typo = function(source = "eu") {


  if (source == "statistik") {
    url = "https://www.statistik.at/gs-atlas/ATLAS_IMAP_WFS/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=ATLAS_IMAP_WFS:DATA_GEM_GDB_DDT_V_PG&outputFormat=application%2Fjson&viewparams=DDT:mv_t0281_topo_stadt_land_2022;YEAR:2022-01-01;V1:CODE_STAT;V2:CODE_STAT;GEN:100;WIENFLAG:1;"
  } else if (source == "eu") {
    url = "https://www.statistik.at/gs-atlas/ATLAS_IMAP_WFS/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=ATLAS_IMAP_WFS:DATA_GEM_GDB_DDT_V_PG&outputFormat=application%2Fjson&viewparams=DDT:mv_t0281_topo_stadt_land_2022;YEAR:2022-01-01;V1:CODE_EK;V2:CODE_EK;GEN:100;WIENFLAG:1;"
  }

  data_raw = fromJSON(url)
  props = data_raw$features$properties %>%
    rename(
      gkz = 1,
      name = 2,
      class_1 = 3,
      class_2 = 4
    ) %>%
    select(-matches("MIN|MAX"))

  return(props)
}
