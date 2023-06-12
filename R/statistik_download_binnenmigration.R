# Download data from Stat Atlas
#' @import jsonlite
#' @import tidyverse
#' @import cli
#' @import tidyr
#' @export
statistik_download_binnen_migration = function(gkz = NULL,
                                     sex = "all",
                                     nationality = "all",
                                     year_min = 2002,
                                     year_max = 2021) {

  if (is.null(gkz)) {
    stop("Must provide a gkz")
  }

  # get the url
  url = build_url(gkz, sex, nationality, year_min, year_max)


  # get the raw data
  data_prepared = statistik_prepare_migration_data(url, gkz)

  # return the data
  return(data_prepared)

}
