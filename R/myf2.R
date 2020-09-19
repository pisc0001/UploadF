#' @title creating myf2
#'
#' @param x
#' @param xk
#' @param xk2
#' @param coef
#'
#' @return
#' @export
#'
#' @examples
myf2 = function(x,xk,xk2,coef){
  coef[1]+coef[2]*(x) + coef[3]*(x-xk)*(x-xk>0)+ coef[4]*(x-xk2)*(x-xk2>0)
}
