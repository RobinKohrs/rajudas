#' Get all my articles
#'
#'
#' @return
#' @export
#'
#' @examples
dst_get_my_articles = function(){
  articles_html = rvest::read_html("httpsc://www.derstandard.at/search?n=&fd=1997-01-01&td=&s=score&query=Robin+Kohrs")

  articles_html %>% rvest::html_elements("article")
}
