#' Format raw lst data sent by the statistik austria
statistik_format_raw_lsv = function(data_year){

  names_l1 = names(data_year) %>%
    as.data.frame() %>%
    mutate(
      across(everything(), function(x){
        case_when(
          str_detect(x, "\\.\\.\\.") ~ NA,
          .default = x
        )
      })
    ) %>%
    rename(sex = 1) %>%
    tidyr::fill(sex) %>%
  pull(1)



  names_l2 = data_year[1,] %>%
    as.list() %>%
    unlist() %>%
    as.data.frame() %>%
    rename(n = 1) %>%
    tidyr::fill(n) %>%
    pull(1)

  names_l3 = data_year[2,] %>%
    as.list() %>%
    unlist() %>%
    as.data.frame() %>%
    rename(n = 1) %>%
    tidyr::fill(n) %>%
    pull(1)

  # actual data
  data = data_year %>%
    slice(3:nrow(.)) %>%
    rename(gkz = 1) %>%
    filter(if_all(everything(), ~ !is.na(.x)))



  dfs = vector("list", length=length(names_l1)-1)
  for (i in seq_along(names_l1)) {
    if(i == 1) next

    # sex
    sex = names_l1[[i]]
    type = names_l2[[i]]
    variable = names_l3[[i]]



    df = data %>%
      select(1, {
        {
          i
        }
      }) %>%
      rename(val = 2) %>%
      mutate(sex = sex,
             type = type,
             variable = variable)

    dfs[[i - 1]] = df
  }

  # bind them togehter
  df = bind_rows(dfs) %>%
    rename(
      gkz = 1
    )

  return(df)


}


#' donwload and prepare migration data
statistik_prepare_migration_data = function(url, gkz_id){
  cli::cli_h2(glue("Downloading and Preparing Data\n"))

  data_raw = fromJSON(url)

  # get the features
  ## relation to each other gemeinde
  features = data_raw$features

  if(length(features) == 0){
    return(NA)
  }

  # unnest the properties col
  features_unnest = features %>% unnest(properties)

  # rename
  data_clean = features_unnest %>%
    dplyr::rename(
      zuz = v1,
      wegz = v2,
      saldo = v3,
      dest_name = name,
      dest_gkz = geo_id,
    ) %>%
    dplyr::select(-id, -geometry, -type) %>%
    dplyr::mutate(
      orig_gkz = gkz_id,
      .before = 1
    )

  return(data_clean)

}


#' Build the url for the migration data from the statisik
#' @import glue
build_url  = function(gkz,sex,nationality,year_min,year_max){

  sex = switch(sex,
               "m" = "1",
               "w" = "2",
               "all" = "1%7C2")


  nationality = switch(nationality,
                       "oe" = "1",
                       "noe" = "2",
                       "all" = "1%7C2")



  url = glue("https://www.statistik.at/gs-atlas/ATLAS_WANDERUNGEN/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=ATLAS_WANDERUNGEN:ATLAS_WANDERUNGEN_SALDO_LIST_GEM&outputFormat=application%2Fjson&viewparams=GKZ:{gkz};DATE:2022-01-01;THRESH_ABS:0;GESCHL:{sex};NOEST:{nationality};JAHR_MIN:{year_min};JAHR_MAX:{year_max}")
  cli::cli_h2(glue("Building URL:\n{url}\n"))

  return(url)

}
