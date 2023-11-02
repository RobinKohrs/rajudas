#' Get geosphere data
#'
#' @param ...
#' @param api_url
#' @param version
#' @param type
#' @param mode
#' @param resource_id
#' @param output_file
#'
#' @return
#' @export
#'
#' @examples
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
  params = params[[1]]
  if (length(params) == 0) {
    stop(glue(
      "The API call must contain some parameters. Check the metadata in
              {url}/metadata
         "
    ))
  }


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
  download.file(final_url, output_file)


  # return the path ---------------------------------------------------------
  return(output_file)

}

