#' A preprocess Function
#'
#' This function allows you to remove special characters and number.
#' @param x Any string.
#' @keywords removeNumbers
#' @export
#' @examples
#' removeNumbers()


 removeNumbers = function(x) { 
     ret1 = gsub("http:[a-zA-Z\\/\\.0-9]+|[a-zA-Z]+|[ 0-9０１-２３４５６７８９]","",x) 
     ret1
 }
