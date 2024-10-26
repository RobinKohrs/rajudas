#' Get the winning party and value
#'
#' @description
#' From a table with one (gemeinde, sprengel...) per line. Each column should be named after one candidate/party
#'
#'
#' @param df The dataframe
#' @param cols a character-vector naming the columns which hold the values
#' @param col_name_winning_party the name of the new column containing the name of the winning party/candidate
#' @param col_name_winning_val the name of the new column containing the value of the winning party/candidate
#'
#' @return
#' @export
#'
#' @examples
elections_get_winning_party_wide = function(df, cols, col_name_winning_party = "winning_party", col_name_winning_val="winning_val"){
  df %>%
    rowwise() %>%
    mutate(
      "{col_name_winning_party}" := names(.)[map(names(df), ~.x%in%cols) %>% unlist][which.max(c_across(cols))],
      "{col_name_winning_val}" := max(c_across(cols), na.rm = T)
    ) -> df

  df

}
