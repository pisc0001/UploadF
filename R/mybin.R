#' @title My Bin
#'
#' @param iter number of iterations
#' @param n trials
#' @param p probability
#'
#' @return A barplot of the proportions and a table of successes
#' @export
#'
#' @examples - this function could be used for 10,000 iterations, 7 trials, and a probability of 0.5. The input would be (iter=10000, n=7, p=0.5). The function would take this information and create a barplot of the proportions and a table of the successes
mybin=function(iter=100,n=10, p=0.5){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  #Make a vector to hold the number of successes in each trial
  succ=c()
  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }
  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))
  #Make a barplot of the proportions
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}
