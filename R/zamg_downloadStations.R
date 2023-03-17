#' Download all the avaiable stations for the ZAMG daily products
#' @importFrom  httr GET content
#' @importFrom  cli cli_h2
#' @importFrom  sf st_as_sf


#' @export
zamg_downloadStations = function(url = "https://dataset.api.hub.zamg.ac.at/v1/station/historical/klima-v1-1d/metadata",
                                outputPath = NULL) {


  if (!is.null(outputPath)) {
    if (file.exists(outputPath)) {
      stations = read.csv(outputPath)
      return(stations)
    }
  }


  cli::cli_h2("Downloading data...")
  metaRaw = httr::GET(url)
  metaContent = httr::content(metaRaw)
  stations = lapply(metaContent$stations, function(x) {
    # replace null with na
    isnull = which(lapply(x, is.null) %>% unlist) %>% unname()
    x[isnull] = NA

    # make df
    df = as.data.frame(x)

    return(df)
  }) %>% do.call("rbind", .)

  stations = st_as_sf(stations, coords = c("lat", "lon"), crs=4326)

  if (!is.null(outputPath)) {
    cli::cli_h2("Writing data...")
    write.csv(stations, outputPath)
    return(stations)
  }

  cli::cli_h2("Returning data in memory")
  return(stations)



}
