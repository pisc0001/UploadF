#' @title myci
#'
#' @param sample of values stored in a variable x
#'
#' @return a 95% confidence interval for the sample
#' @export
#'
#' @examples - this function can take in any sample as an input and will provide the 95% confidence interval. For example, the input could be set.seed(25) and x = rnorm(24, mean = 5, sd = 10).
myci = function(sample = x){
  int = t.test(x, conf.level = 0.95)$conf.int
  return(int = int)
}
