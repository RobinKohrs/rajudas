#' send request to download data from the zamg API
#'
#' @import httr
#' @import tidyverse
#'
#'

zamg_sendRequest = function(url){

  cli::cli_h2(glue("Sending request to: {url}"))

  # send request
  rawResponse = httr::GET(url)

  if(!rawResponse$status_code == 200){
    stop("Something went wrong in the request")
  }

  # get the content
  responseContent = httr::content(rawResponse)

  # return the content
  return(responseContent)

}

