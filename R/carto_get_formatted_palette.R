#' get one or more color palettes from Carto
#'
#' @import rvest
#'

cart_get_formatted_palette = function(carto_palette =NULL){
  if (is.null(carto_palette)) {
    stop("NAAA, dont do that...")
  }

  # url to carto palettes
  url = "https://carto.com/carto-colors/"

  # get the palettes
  html = read_html(url)

  # schems
  schemas = html %>% html_elements(".grid.container")

  map(seq_along(schemas), function(i){

    s = schemas[[i]]
    nm = s %>% html_text() %>% janitor::make_clean_names()

  })

  # gave up, there is a much simpler solution...
  # Just copy the json!

}
