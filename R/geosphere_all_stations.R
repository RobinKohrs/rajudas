
geosphere_all_running_stations = function(){

  stations_all = rajudas::geosphere_get_stations()

  ugi = stations_all %>% pull(group_id) %>% unique

  ugi %in% stations_all$id

}
