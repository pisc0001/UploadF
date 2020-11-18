#' @title myncurve
#'
#' @param a input x
#' @param mu mean
#' @param sigma standard deviation
#'
#' @return The function displays the curve, shades the area between the curve and x axis from -inf to x=a and calculates the area in a list
#' @export
#'
#' @examples - This function could take an input where x is 50, the mean is 10, and the standard deviation is 5. It would then use the function and dnorm to find the area under the curve and print that as well as the graph with the area shaded.
myncurve = function(a,mu,sigma){
  curve(dnorm(x, mean = mu, sd = sigma), xlim = c(mu-3*sigma, mu+3*sigma))
  xcurve = seq(-1000,a,length = 1000)
  ycurve = dnorm(xcurve,mu,sigma)
  polygon(c(-1000, xcurve, a), c(0,ycurve,0), col = "pink")
  area = pnorm(a,mu,sigma) - pnorm(-1000000000,mu,sigma)
  area = round(area,4)
  area
}
