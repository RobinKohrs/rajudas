#' Download Arctic ice extent
#'
#' @description
#' from here \link[https://nsidc.org/arcticseaicenews/charctic-interactive-sea-ice-graph/]
#' For now only for the north (artic) and with 5-day moving average
#'
#'
#' @param years
#'
#' @return
#' @export
#'
#' @examples
artic_sea_ice = function(years=NULL, pole="north", window=5){

  if(is.null(years)){
    stop("You need to provide some years...")
  }

  # data for the years
  data_all_years = map(years, function(y){
    cli::cli_h2(glue::glue("Data for year: {y}"))
    url = glue::glue("https://nsidc.org/api/seaiceservice/extent/{pole}/filled_averaged_data/{y}?index=doy&smoothing_window={window}")
    data_raw = jsonlite::fromJSON(url)

    dates = map(names(data_raw), function(d){
      as.Date(as.numeric(d)-1, origin=glue::glue("{y}-01-01"))
    }) %>% do.call("c", .)

    data = data_raw %>% unname() %>% unlist

    df = data.frame(date = dates,
                    ice_extent_mi_sqkm = data,
                    variable = "datapoint")

  }) %>% purrr::list_rbind()

  # interquantile range
  url_quantiles = "https://nsidc.org/api/seaiceservice/extent/{pole}/quantiles?smoothing_window={window}"
  data_quantiles_raw = jsonlite::fromJSON(url_quantiles)

  cli::cli_h2(glue::glue("Data for aggregates"))
  data_aggregates = imap(data_quantiles_raw, function(data, q){

    quantile = glue::glue("q{q}_1981_to_2010")
    dates = map(names(data), function(d){
      as.Date(as.numeric(d)-1, origin="2000-01-01")
    }) %>% do.call("c", .)
    dd = data %>% unname() %>% unlist


    df = data.frame(date = dates, ice_extent_mi_sqkm = dd)
    df[["variable"]] = quantile
    return(df)
  }) %>% purrr::list_rbind()

  data_all = bind_rows(
    data_all_years,
    data_aggregates
  )


  return(data_all)

}
