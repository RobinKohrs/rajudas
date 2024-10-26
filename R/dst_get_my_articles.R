#' Get all my articles
#'
#'
#' @return
#' @export
#'
#' @examples
dst_get_my_articles = function(){
  articles_html = rvest::read_html(here::here("dst/articles.html"))
  article_links = articles_html %>% rvest::html_elements("article")

  as = map(article_links, function(a){
    rel_link = a %>% rvest::html_element(".teaser-inner > a") %>% rvest::html_attr("href")
    abs_link = glue::glue("https://www.derstandard.at/{rel_link}")
    date = a %>% rvest::html_elements("dst-rl-timestamp") %>% rvest::html_attr("date") %>% lubridate::ymd_hms(.,tz = "Europe/Berlin")
    name = a %>% rvest::html_element("h1") %>% rvest::html_text()

    res = list(
      link=abs_link,
      date=date,
      name=name
    )
    return(res)

  }) %>% bind_rows()

  # manuelle article
  manuelle_articles = list(
    list(
      link = "https://www.derstandard.at/story/3000000241922/georgien-vor-der-entscheidung-europa-oder-russland",
      date = as.Date("2024-10-25"),
      name = "Wahl Georgien 2024"
    )
  )

  all = bind_rows(as, manuelle_articles)


  all %>%
    write_sheet(
      ss = gs4_get(
        "https://docs.google.com/spreadsheets/d/15f-vc-7I6vrTJhl1nB2dR4Kth7kutB5n9oibV7RkFo4/edit?gid=0#gid=0" # Replace the access link to the spreadsheets
      ),
      sheet = "articles"
    )




}
