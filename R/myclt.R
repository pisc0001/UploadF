#' @title My CLT
#'
#' @param n
#' @param iter
#' @param a bottom limit
#' @param b upper limit
#'
#' @return a histogram of the sums
#' @export
#'
#' @examples
myclt=function(n,iter,a=0,b=5){
  y=runif(n*iter,a,b) #A
  data=matrix(y,nr=n,nc=iter,byrow=TRUE) #B
  sm=apply(data,2,sum) #C
  h=hist(sm,plot=FALSE)
  hist(sm,col=rainbow(length(h$mids)),freq=FALSE,main="Distribution of the sum of uniforms")
  curve(dnorm(x,mean=n*(a+b)/2,sd=sqrt(n*(b-a)^2/12)),add=TRUE,lwd=2,col="Blue")
  sm
}
w=myclt(n=50,iter=10000,a=5,b=10) #D
