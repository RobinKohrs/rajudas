#' get population data for Austrian gemeinden
#'
#' @import tidyverse
#' @import readODS

#' @export
oe_pop_data = function(url = "https://www.statistik.at/fileadmin/pages/405/Bev_zu_Jahresbeginn_Gebietseinheiten_Zeitreihe.ods"){

  # download data
  cli::cli_h2(glue("Downloading data from: {url}"))
  dest_file  = tempfile()
  res = download.file(url, destfile = dest_file)

  # read data
  cli::cli_h2(glue("Reading data"))
  data = readODS::read_ods(dest_file, sheet="Gemeinde", skip=1)

  # little cleaning
  headers = c("gkz", "name", 2002:2023)
  data = data %>% slice(1:nrow(.)) %>% setNames(headers)

  return(data)

}
