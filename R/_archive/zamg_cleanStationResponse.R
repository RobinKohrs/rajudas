#' Prepare the data from a call to the station data


zamg_cleanStationData = function(data){

  # check if length of features is 1 (is this related to the number of stations??)
 features = data$features
 if(!length(features) == 1){
   stop("The respose has a feature-length of > 1 -> Further investigation needed")
 }

 # get the timestamps
 timestamps = data$timestamps %>% unlist()

 # get the number of parameters
 parameters = features[[1]]$properties$parameters
 nParameters = length(parameters)
 namesParamters = names(parameters) %>% paste0(collapse = ", ")
 cli::cli_h2(glue("Extracting data for {nParameters} variables: {namesParamters}"))

 dataEachParam = imap(parameters, function(p, nm){

   dataParam = p$data
   # if there is null in the data, replace with NA
   dataParamNoNULL = map(dataParam, function(d){
     if(is.null(d)) return(NA)
     return(d)
   }) %>% unlist

   nmParam = p$name
   unitParam = p$unit

   df = data.frame(
     name = nmParam,
     unit = unitParam,
     time = timestamps,
     data = dataParamNoNULL
   )

   return(df)

 })









}
