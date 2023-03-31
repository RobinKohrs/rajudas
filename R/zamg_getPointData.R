#' Get data from Zamg Station data
#' @import glue
#' @import httr
#'
#' @export


zamg_get_point_data = function(
  dataset = "1d",
  stationId = NULL,
  parameters = NULL,
  start = NULL,
  end = NULL
){



  if (any(unlist(lapply(
    list(dataset,
         stationId,
         parameters,
         start,
         end), is.null
  ))))  {
    stop("Parameter missing")
  }


  # format the paramers if more than one ------------------------------------
  if (length(parameters) > 1) {
    if (!is.character(parameters)){
      stop("paramters must be a characeter vector")
    }
    parameters = paste0(parameters, collapse = ",")
  }


  # build the url -----------------------------------------------------------
  url = glue("https://dataset.api.hub.zamg.ac.at/v1/station/historical/klima-v1-{dataset}?parameters={parameters}&start={start}&end={end}&station_ids={stationId}")


  # download the data -------------------------------------------------------
  response = zamg_sendRequest(url)


  # clean the data ----------------------------------------------------------
  dataClean = zamg_cleanStationData(response)


  # return cleanded data ----------------------------------------------------
  return(dataClean)

}

