#' Binary search
#'
#' @param x key
#' @param y vector
#' @param startIndex default 1
#' @param endIndex length of y
#' @param tol double tolerance 
#'
#' @return index of y
#'
#' @examples
#' binary_search(2, 1:10)
#' binary_search(2.1, 1:10)
#' binary_search(2.8, 1:10)
#' binary_search(3, 1:10)
#' binary_search(3.5, 1:10)
binary_search <- function(x, y, startIndex = 1, endIndex= length(y), tol = sqrt(.Machine$double.eps)){
  
  while (startIndex <= endIndex) {
    midIndex <- as.integer(ceiling((startIndex+ endIndex) / 2))
    midValue <- y[midIndex]
    
    if (midValue < x - tol) {
      startIndex = midIndex + 1
    } else if (midValue > x + tol) {
      endIndex = midIndex - 1;
    } else {
      return(midIndex)
    }
  }
  return(startIndex)
}
