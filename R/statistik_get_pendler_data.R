#' get the pendler saldo for a gemeinde to all other gemeinden
#'
#' @export

statistik_get_pendler_data = function(gkz, year=2020, no_binnenpendler = T){

  # url
  url = glue::glue("https://www.statistik.at/gs-atlas/ATLAS_PENDLER/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=ATLAS_PENDLER:ATLAS_PENDLER_SALDO_LIST_GEM&outputFormat=application%2Fjson&viewparams=GKZ:{gkz};DATE:{year}-10-31;THRESH_ABS:0")
  cli::cli_inform(glue("URL:\n{url}"))

  # raw data
  data_raw = fromJSON(url)

  props = data_raw$features$properties

  # rename the cols
  props = props %>%
    rename(
      dest_name = name,
      dest_gkz = geo_id,
      einpendler = v1,
      auspendler = v2,
      saldo = v3
    )

  # remove own gemeinden
  if(no_binnenpendler){
    cli::cli_alert("Removing pendler from own gemeinde...")
    props = props %>%
      filter(dest_gkz != gkz)
  }


  return(props)

}
