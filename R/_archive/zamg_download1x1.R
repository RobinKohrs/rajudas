#' Download Zamg 1 by 1 km Gridded Spartacus data
#'
#' @description More info \link[here]{https://dataset.api.hub.zamg.ac.at/v1/docs/quickstart.html}.
#' Test the API \link[here]{https://dataset.api.hub.zamg.ac.at/v1/openapi-docs#/grid/Current_Grid_Data_grid_current__resource_id__get}
#'
#'
#' @param parameter Either "Tx, Tm, RR"
#' @param start_date start date in yyyy-mm-dd
#' @param end_date end date in yyyy-mm-dd
#' @param bb a bounding box. Either a \code{sf-object} (then the bounding box of the sf object will be computed),
#' or a \code{bbox-object}
#' @param outputFilename The output file name (must have the ending \code{.nc})

#' @importFrom stars read_stars

#' @export
zamg_get_1x1 = function(
    format = "netcdf",
    parameter = NULL,
                            start_date = as.Date("2023-03-20"),
                            end_date = Sys.Date(),
                            bb = NULL,
                            outputFilename = NULL
                            ) {



  # which param -------------------------------------------------------------
  if(is.null(parameter)){
    stop("You need to provide on of the following paramters:
         [RR - Daily precipitation Sum],
         [Tx - Daily maximum of air temperature],
         [Tn - Daily minimum of air temperature]")
  }

  if(any(!parameter %in% c("Tx", "RR", "Tn"))){
    stop("You provided a parameter that is not available")
  }

  if(length(parameter) > 1){
    parameter = paste0(parameter, collapse = ",")
  }


  # bounding box ------------------------------------------------------------
  if (is.null(bb)) {
    bb = c("46.351662,9.435046,49.129654,17.203553")
  } else if (!is.character(bb)) {
    if ("sf" %in% class(bb)) {
      bb = st_bbox(bb)
    }
    bb = bb[c(2, 1, 4, 3)]
    bb = paste0(bb, collapse = ",")
  }


  # outputfile name ---------------------------------------------------------
  if(is.null(outputFilename)){
    stop("You need to provide a file name")
  }



  # build url ---------------------------------------------------------------
  url = glue("https://dataset.api.hub.zamg.ac.at/v1/grid/historical/spartacus-v1-1d-1km?parameters={parameter}&start={start_date}&end={end_date}&bbox={bb}&output_format={format}&filename=temp")

  # outputFile
  cli::cli_inform(glue("Downloading data for: {start_date} to {end_date}"))
  download.file(url, destfile = outputFilename)



}

