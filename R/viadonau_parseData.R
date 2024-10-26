viadonau_parseData = function(url="https://www.viadonau.org/planetweb/pegel2015/csvdata.php?id=KORN"){
  data_raw = readLines(url)
  lines = imap(data_raw, function(d, i) {
    if (i == 1) {
      return()
    }

    time = d %>% str_trim() %>% str_sub(1, 16) %>% as.POSIXct()
    val = str_sub(d, 17, nchar(d)) %>% str_extract("\\d+")

    return(list(time = time,
                val = val))


  }) %>% bind_rows()

  return(lines)

}
