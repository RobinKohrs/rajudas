#' Get the gleichstellungsindex values
#'
#' @param url Url to the xlsx file at the sora page
#' @export


sora_get_gleichstellung = function(url="https://www.sora.at/fileadmin/downloads/projekte/2022_SORA_Datensatz_Staedtebund_Gleichstellungsindex_2021_Dimensionsindizes.xlsx"){

  tmp_file = paste0(tempfile(), ".xlsx")
  download.file(url, tmp_file)
  data_raw = read_xlsx(tmp_file, sheet = "Daten", skip = 1) %>% janitor::clean_names()
  return(data_raw)


}
