#' Get ZAMG Variables
#'
#'
#'
#' @export


getZamgVariables = function(
    url = "https://dataset.api.hub.zamg.ac.at/v1/station/historical/klima-v1-1d/metadata",
    outputPath = NULL

){
  if (!is.null(outputPath)) {
    if (file.exists(outputPath)) {
      df = read.csv(outputPath)
      return(df)
    }
  }


  cli::cli_h2("Downloading meta data...")
  metaRaw = httr::GET(url)
  metaContent = httr::content(metaRaw)

  df = data.frame(do.call("rbind", metaContent$parameters))

  if (!is.null(outputPath)) {
    cli::cli_h2("Writing data...")
    write.csv(df, outputPath)
    return(df)
  }

  cli::cli_h2("Returning data in memory")
  return(df)

}
