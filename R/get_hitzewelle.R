#' Identify Hitzewellen
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
get_hitzewelle = function(x=NULL, defintion="kysely"){
  if (is.null(x)) {
    stop("Must provide data")
  }


  current_avg = 0
  current_vals = c()
  in_heatwave = F
  heatwave_days = vector(length=length(x))
  heatwave_three_days = c()

  x = c(30,30,31,29,30,30,30,24.9)


  for(i in 1:length(x)){
   val = x[[i]]

   # when less than 25 break out
   if(val < 25){
     heatwave_days[[i]] = F
     in_heatwave = F
     heatwave_three_days = c()
     next
   }

   # if temp > 30
   if(val >= 30){
     heatwave_three_days = c(heatwave_three_days, i)

     if(length(heatwave_three_days) >= 3){
       in_heatwave = T
       heatwave_days[heatwave_three_days] = T
     }
   }

   # if temp < 30
   if(val < 30){
     # if average of current heawave > 30 no prob
     indexes_current_heatwave = min(heatwave_three_days):i
     vals_current_heatwave=x[indexes_current_heatwave]
     if(mean(vals_current_heatwave) >= 25){
       heatwave_days[i] = T
     }
   }



  }

  return(heatwave_days)
}

df = data.frame(
  day = 1:20,
  temp = sample(25:40, 20, replace = T)
)












