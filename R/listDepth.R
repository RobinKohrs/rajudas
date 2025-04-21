#' Determine the max depth of a list in R
#'
#' @param this a list
#' @param thisdepth default depth if \code{this} is not a list
#'
#' @return
#' @export
#'
#' @examples
list.depth = function(this,thisdepth=0){
  if(!is.list(this)){
    return(thisdepth)
  }else{
    return(max(unlist(lapply(this,list.depth,thisdepth=thisdepth+1))))
  }
}
