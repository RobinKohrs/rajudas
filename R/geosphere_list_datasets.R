geosphere_list_datasets = function(url="https://dataset.api.hub.geosphere.at/v1/datasets"){

  # download data -----------------------------------------------------------
  d = fromJSON(url)
  # make dataframe ----------------------------------------------------------
  df = bind_rows(d)
  df %>%
    mutate(
      name = basename(url),
      metadata = glue("{url}/metadata")
    ) -> df

  parameters_url = "https://dataset.api.hub.geosphere.at/v1/openapi-docs#/grid/Historical_Grid_Data_grid_historical__resource_id__get"
  cli::cli_h2(glue("More help for the exact names of the parameters here:\n{parameters_url}"))
  return(df)
}
