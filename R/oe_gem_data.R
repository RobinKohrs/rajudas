#' get the gemeinde geodata for a specific year

#' @param year
#'
#' @param base_url url to where the website with all the years is
#' @param dest_dir folder where the unzipped data will be
#' @param dest_file the file to where you want to download the zipped gemeinden
#'
#' @import cli
#' @import stringr
#' @import sf
#' @export


oe_gem_data = function(year=2023,
                       base_url="https://data.statistik.gv.at/data/OGDEXT_GEM_1_STATISTIK_AUSTRIA_",
                       dest_file = NULL,
                       dest_dir = tempdir(),
                       wien_union = F,
                       force = F
                       ){

  if(is.null(dest_file)){
    dest_file = paste0(dest_dir, "/", "gemeinden_", year, ".gpkg")
  }

  if(file.exists(dest_file) && !force){
    cli::cli_alert("File already exists...")
    raw_shp = read_sf(dest_file)
    return(raw_shp)
  }


  # url to data
  url = glue::glue("{base_url}{year}0101.zip")

  # destfile
  destfile_zip = paste0(dest_dir, "/gemeinden_oe", year, ".zip")

  # download zip
  download.file(url, destfile = destfile_zip)

  # unzip
  unzip(destfile_zip, exdir = dest_dir)

  # read shapefile
  shp_file = dir(dest_dir, full.names = T) %>% stringr::str_subset(glue("{year}.*\\.shp"))
  raw_shp = read_sf(shp_file)

  # which is the id col
  col_id = str_subset(names(raw_shp), "id")
  # which is the id col
  col_name = str_subset(names(raw_shp), "name")


  # find if wien is uniioned or not
  if(wien_union) {
    raw_shp = raw_shp %>%
      mutate({
        {
          col_id
        }
      } := if_else(str_detect(get(col_id), "^9"), "90001", get(col_id)),
      g_name = if_else(str_detect(get(col_id), "^9"), "Wien", g_name)) %>%
      group_by(across(all_of(col_id)), g_name) %>%
      summarise() %>%
      ungroup()
  }






  # write it out
  write_sf(raw_shp, dest_file)

  return(raw_shp)

}
