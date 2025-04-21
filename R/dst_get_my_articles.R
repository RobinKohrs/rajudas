#' Get all my articles
#'
#'
#' @return
#' @export
#'
#' @examples
dst_get_my_articles = function(downloaded_html_file=NULL){

  if(is.null(downloaded_html_file)){

    # check to find it in default location
    downloaded_html_file = system.file("robinkohrs_articles.html", package = "rajudas")

    if(file_path == ""){
      cli::cli_alert_danger("No HTML file specified")
    }

    # read it in
    rvest::read_html(downloaded_html_file, encoding = "UTF-8") %>%
      rvest::html_elements("article") -> articles

    article_with_attribtues = purrr::map(articles, function(article){
      get_article_attributes(article)
    }) %>% dplyr::bind_rows()

    return(article_with_attribtues)

  }

}

get_article_attributes = function(article){

  article_attributes = list(
    title = article %>%
      rvest::html_element(".teaser-title") %>%
      rvest::html_text() %>%
      gsub("[^A-Za-z0-9 öäüÖÜÄ/-]", "", .),

    subtitle = article %>%
      rvest::html_element(".teaser-subtitle") %>%
      rvest::html_text() %>%
      gsub("[^A-Za-z0-9 öäüÖÜÄ/-]", "", .),

    postings = article %>%
      rvest::html_element(".teaser-postingcount") %>%
      rvest::html_text() %>%
      stringr::str_extract("[0-9]+") %>%
      as.numeric() %>% ifelse(is.na(.), 0, .),

    link = article %>%
      rvest::html_element("a") %>%
      rvest::html_attr("href") %>%
      paste0("https://www.derstandard.at", .),

    img_src = article %>%
      rvest::html_element("img") %>%
      rvest::html_attr("data-lazy-src")
  )

  return(article_attributes)


}
