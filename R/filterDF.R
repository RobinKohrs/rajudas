#' Filter datataframe
#'
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @importFrom rlang enquo


fi = function(df, col, val){
  col = rlang::enquo(col)
  filt = df %>%
    filter(
      str_detect(
       !!col, val
      )
    )
  return(filt)
}

