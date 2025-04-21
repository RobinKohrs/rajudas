#' Get data from geosphere data
#'
#' @description
#' From the arguments \code{api_url}, \code{version}, \code{type}, \code{mode} and \code{ressource_id} a
#' url is constructed as described \link[here](https://dataset.api.hub.geosphere.at/v1/docs/getting-started.html).
#' In \code{...}-argument must be specified the parameters for each endpoint. These parameters are a little tricky, but can
#' roughly be found partially in the \link[base-url](https://dataset.api.hub.geosphere.at/v1/timeseries/historical/spartacus-v2-1m-1km)
#'  of the endpoint and the corresponding \link[metadata-page](https://dataset.api.hub.geosphere.at/v1/timeseries/historical/spartacus-v2-1m-1km/metadata)
#'  for that endpoint. To my undestanding the url-paramter-names are int he base-url and the options they can
#'  take are in the metadata-page
#'
#'
#' @param ... Parameters for that specific endpoint
#' @param api_url The base url to the server of geosphere. Check \link[here](https://dataset.api.hub.geosphere.at/v1/docs/getting-started.html)
#' @param version The version of the api. By default \code{"v1"}.
#' @param type Either \code{"grid"}, \code{"timeseries"} or \code{"station"}
#' @param mode Either \code{"historical"}, \code{"current"} or \code{"forecast"}
#' @param resource_id One listed \link[here](https://dataset.api.hub.geosphere.at/v1/docs/user-guide/resource.html#resources) or
#' \link[here](https://dataset.api.hub.geosphere.at/v1/datasets)
#' @param output_file
#'
#' @examples{
#'  path_data = rajudas::geosphere_get_data(
#'   list(
#'     parameters = c("tl_mittel"),
#'     start = start,
#'     end = end,
#'     station_ids = id,
#'     output_format = "csv"
#'   ),
#'   type = "station",
#'   mode = "historical",
#'   resource_id = "klima-v2-1m"
#' )
#'
#' # Get hourly data and read it in directly
#'  data = rajudas::geosphere_get_data(
#'       list(
#'         parameters = c("tl"),
#'         start = "2025-04-06T05:00:00",
#'         end = "2025-04-06T17:00:00",
#'         station_ids = 5925,
#'         output_format = "csv"
#'       ),
#'       type = "station",
#'       mode = "historical",
#'       resource_id = "klima-v2-1h"
#'       ) %>% read_csv()
#' }
#'
#' @return The file to where the data was saved
#' @export
#'
geosphere_get_data = function(...,
                              api_url = "https://dataset.api.hub.geosphere.at",
                              version = "v1",
                              type = "grid",
                              mode = "historical",
                              resource_id = "spartacus-v2-1d-1km",
                              output_file = tempfile()) {




  # build base url -----------------------------------------------------------
  url = paste(api_url, version, type, mode, resource_id, sep = "/")

  # get params --------------------------------------------------------------
  params = list(...)
  if (length(params) == 0) {
    stop(glue(
      "The API call must contain some certain parameters. Check the url for the names of the params here
              {cli::style_hyperlink(url, url)}
      the available options for these parameters in the metadata page
              {cli::style_hyperlink(url, url)}/metadata
       or all the available datasets at
              {cli::style_hyperlink('https://dataset.api.hub.geosphere.at/v1/datasets', 'https://dataset.api.hub.geosphere.at/v1/datasets')}
         "
    ))
  }
  params = params[[1]]


  # add params to url -------------------------------------------------------
  q = lapply(seq_along(params), function(i) {
    n = names(params)[[i]]
    v = params[[i]]
    p = glue("{n}={v}")
  }) %>% unlist %>% paste0(collapse = "&")


  # add to url --------------------------------------------------------------
  final_url = glue("{url}?{q}")


  # download data -----------------------------------------------------------
  print(glue("Final url: {final_url}"))
  download.file(final_url, output_file, timeout=120)


  # return the path ---------------------------------------------------------
  return(output_file)
}

