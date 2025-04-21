#' Get all the datasets form Geosphere
#'
#' @description
#' Basically the same information as on the \link[ressource-page](https://dataset.api.hub.geosphere.at/v1/docs/)
#'
#'
#' @param url URL to the datasets. Currently \code[https://dataset.api.hub.geosphere.at/v1/datasets]
#'
#' @return a dataframe with all the info to the datasets
#' @export
#'
geosphere_get_datasets = function(url="https://dataset.api.hub.geosphere.at/v1/datasets", html=F){

  # download data -----------------------------------------------------------
  d = jsonlite::fromJSON(url) %>% unname()
  # make dataframe ----------------------------------------------------------
  df = do.call("rbind", lapply(d, as.data.frame))
  df %>%
    dplyr::mutate(
      name = basename(url),
      metadata = glue::glue("{url}/metadata")
    ) -> df

  parameters_url = "https://dataset.api.hub.geosphere.at/v1/openapi-docs#/grid/Historical_Grid_Data_grid_historical__resource_id__get"
  cli::cli_h2(glue::glue("More help for the exact names of the parameters here:\n{parameters_url}"))
  return(df)
}
