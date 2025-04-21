#' Process the statistik lst data
#'
#' @param dir_with_excels
#' @param sex Frauen, Männer, Zusammen
#' @param type Bruttobezüge, Nettobezüge, SV-Beiträge, einbehaltene Lohnsteuer
#' @param variable Anzahl, Median, Mittelwert, P10, P20, P30, P40, P60, P70, P80, P90, Summe
#'
#' @import readxl
#' @export
statistik_read_lst = function(
   dir_with_excels = "~/projects/dst/daten/DATEN_LST_STATISTIK/Standard (Robin Kohrs)/",
   sex="Zusammen",
   type="Nettobezüge",
   variable = "Median"
){


  # list all the excels -----------------------------------------------------
  excel_files = dir(dir_with_excels, ".*Gem.*\\.xls[x]?$", full.names = T)
  if(!length(excel_files) >= 18){
    stop("There must be at least 18 files... Maybe the wrong folder?!")
  }


  # read the years from the files -------------------------------------------
  years = str_match(basename(excel_files), "\\d{2,4}") %>% .[,1]

  data_all = purrr::map(seq_along(excel_files), function(i){

    # the year of the data
    year = years[[i]]
    cli::cli_inform(glue("Reading data for year: {year}"))

    # read the data
    data_year = readxl::read_excel(excel_files[[i]], skip = 1)

    # bring the data into long format
    clean_data = statistik_format_raw_lsv(data_year)

    # filter
    clean_data %>%
      dplyr::filter(
        .data$sex %in% .env$sex &
        .data$type %in% .env$type &
        .data$variable %in% .env$variable
      ) -> data_filtered

    data_filtered[["year"]] = year

    return(data_filtered)

  })

  return(data_all)

}
