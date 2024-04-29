#' Get Linearly interpolated color from Array of colors
#'
#' @param colors a vector of colors
#' @param df a dataframe
#' @param col  a numeric column
#'
#' @return
#' @export
#'
#' @examples
get_linear_colors = function(df, col, colors){
  cr = colorRamp(colors)
    df %>%
    mutate(r = scales::rescale(.data[[col]]),
           color = rgb(cr(r), maxColorValue = 255)) %>%
    select(-r) -> df

    return(df)

}
