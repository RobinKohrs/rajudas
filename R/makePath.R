#' Make the directory until the file
#'
#' @param path
#'
#' @return the path
#' @export
#'
makePath = function(path){
  dirFromPath = dirname(path)
  if(!dir.exists(dirFromPath)){
    dir.create(dirFromPath, recursive = T)
  }
  return(path)
}
