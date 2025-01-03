#' Get Global CO2 Data
#'
#' @param paths Paths to the daily, monthly and yearly data
#'
#' @return
#' @export
#'
#' @examples
co2_get_global_data = function(paths = list(
  path_daily_data = "https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_trend_gl.csv",
  path_monthly_data = "https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_mm_gl.csv",
  path_yearly_data = "https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_annmean_gl.csv"
)) {


  data_daily_month_yearly = purrr::imap(paths, function(p, nm) {
    if (nm == "path_daily_data") {
      cli::cli_h2("Getting daily data")
      d = readr::read_csv(
        p,
        skip = 36
        # col_names = c("year", "month", "day", "decimal_date", "daily_avg")
      )
      dd = d %>%
        mutate(date = as.Date(glue::glue("{year}-{month}-{day}"))) %>%
        select(date, daily_avg_smooth = smoothed, daily_avg_trend=trend)
      return(dd)
    }

    if (nm == "path_monthly_data") {
      cli::cli_h2("Getting monthly data")
      d = read_csv(p, skip = 38) %>% janitor::clean_names()
      dm = d %>%
        mutate(date = as.Date(glue::glue("{year}-{month}-01"))) %>%
        rename(monthly_avg = average, monthly_trend=trend) %>%
        select(date, monthly_avg, monthly_trend)
      return(dm)
    }

    if (nm == "path_yearly_data") {
      cli::cli_h2("Getting yearly data")
      dy = read_csv(p, skip = 37) %>% janitor::clean_names() %>%
        mutate(date = as.Date(glue::glue("{year}-12-01"))) %>%
        select(date, yearly_avg = mean)

      return(dy)
    }

  })

  data_all = purrr::reduce(data_daily_month_yearly, dplyr::full_join)

  return(data_all)

}

