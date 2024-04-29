#' get population data for Austrian gemeinden
#'
#' @import tidyverse
#' @import readODS

#' @param admin Either (g=gemeinde | b=bezirk | bl=bundesland | n=Nuts)
#' @param url
#' @param wien_union If adming=g, if the wiener bezirke should be unioned or not
#'
#' @export
oe_pop_data = function(years = NULL,
                       admin = "gemeinden",
                       url = "https://www.statistik.at/fileadmin/pages/405/Bev_zu_Jahresbeginn_Gebietseinheiten_Zeitreihe.ods",
                       wien_union = F) {

  # download data
  cli::cli_h2(glue("Downloading data from: {url}"))
  dest_file  = tempfile()
  res = download.file(url, destfile = dest_file)

  # which sheet
  sheet = switch (
    admin,
    "g" = "Gemeinde",
    "G" = "Gemeinde",
    "gem" = "Gemeinde",
    "Gemeinde" = "Gemeinde",
    "gemeinden" = "Gemeinde",
    "b" = "Bezirk",
    "B" = "Bezirk",
    "bezirke" = "Bezirk",
    "n" = "Nuts",
    "N" = "Nuts",
    "nuts" = "Nuts",
    "bl" = "Bundesland",
    "Bl" = "Bundesland",
    "bundeslaender" = "Bundesland"
  )


  # read data
  cli::cli_h2(glue("Reading data"))

  data = data.frame()

  if (sheet == "Gemeinde") {
    data = readODS::read_ods(dest_file, sheet = sheet, skip = 1)
    names_old = names(data)
    names_old_years = str_subset(names_old, "^[0-9]+$")
    names_new = c("gkz", "name", names_old_years)
    names(data) = names_new
    data %>% filter(if_all(everything(),  ~ !is.na(.x))) -> data


    # if you do not want wien separate
    if (!wien_union) {
      # else get the data from the Bezirke sheet
      data_bez = readODS::read_ods(dest_file, sheet = "Bezirk", skip = 1)
      names_old = names(data_bez)
      names_old_years = str_subset(names_old, "^[0-9]+$")
      names_new = c("bkz", "name", names_old_years)
      names(data_bez) = names_new

      # get all the districts in vienna
      data_wien = data_bez %>%
        filter(str_detect(bkz, "^9")) %>%
        mutate(
          gkz = glue("{bkz}01")
        )

      bind_rows(data, data_wien) -> data
    }

  }

  if (sheet == "Bezirk") {
    data_bez = readODS::read_ods(dest_file, sheet = "Bezirk", skip = 1)
    names_old = names(data_bez)
    names_old_years = str_subset(names_old, "^[0-9]+$")
    names_new = c("bkz", "name", names_old_years)
    names(data_bez) = names_new
    data = data_bez
  }

  if (sheet == "Bundesland") {
    data_bl = readODS::read_ods(dest_file, sheet = "Bundesland", skip = 1)
    names_old = names(data_bl)
    names_old_years = str_subset(names_old, "^[0-9]+$")
    names_new = c("blkz", "name", names_old_years)
    names(data_bl) = names_new
    data = data_bl %>% filter(str_detect(blkz, "^AT|[0-9]+$"))
  }


  if (!is.null(years)) {
    data = data %>%
      select(1, 2, all_of(as.character(years)))
  }

  return(data)
}
