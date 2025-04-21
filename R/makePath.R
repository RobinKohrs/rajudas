#' Make the directory until the file
#'
#' @param path
#'
#' @return the path
#' @export
#'
makePath = function(path) {
  # if is file
  is_file = str_detect(basename(path), "\\.")
  if (is_file) {
    dirFromPath = dirname(path)
    if (!dir.exists(dirFromPath)) {
      dir.create(dirFromPath, recursive = T)
    }
    return(path)
  }else{
    dir.create(path, recursive = T)
    return(path)
  }

}
