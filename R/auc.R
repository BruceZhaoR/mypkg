#' Area under the ROC curve (AUC)
#'
#' \code{auc} computes the area under the receiver-operator characteristic curve (AUC).
#' 
#' \code{auc} uses the fact that the area under the ROC curve is equal to the probability
#' that a randomly chosen positive observation has a higher predicted value than a
#' randomly chosen negative value. In order to compute this probability, we can
#' calculate the Mann-Whitney U statistic. This method is very fast, since we
#' do not need to compute the ROC curve first.
#'
#' @param predicted A numeric vector of predicted values, where the smallest values correspond
#'                  to the observations most believed to be in the negative class
#'                  and the largest values indicate the observations most believed
#'                  to be in the positive class. Each element represents the
#'                  prediction for the corresponding element in \code{actual}.
#' @export
#' @examples
#' actual <- c(1, 1, 1, 0, 0, 0)
#' predicted <- c(0.9, 0.8, 0.4, 0.5, 0.3, 0.2)
#' auc(actual, predicted)
auc <- function(actual, predicted) {
  if (length(actual) != length(predicted)) {
    msg <- "longer object length is not a multiple of shorter object length"
    warning(msg)
  }
  r <- rank(predicted)
  n_pos <- as.numeric(sum(actual == 1))
  n_neg <- length(actual) - n_pos
  return((sum(r[actual == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg))
}

