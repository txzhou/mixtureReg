# methods for mixtureReg

#' Sort by X Coordinates and Add Line to a Plot
#'
#' Rearrange X and Y coordinates before calling "lines()" function.
#'
#' @param x X coordinate vectors of points to join.
#' @param y Y coordinate vectors of points to join.
#' @param ...	Further graphical parameters.
orderedLines <- function(x, y, ...) {
  # a helper function used in plotting
  xOrder <- order(x)
  lines(x = x[xOrder], y = y[xOrder], ...)
}

#' Plot Fit and Mixing Probability of a mixtureReg Object
#'
#' S3 plot method for class 'mixtureReg'.
#'
#' @param mixtureModel mixtureReg object, typically result from 'mixtureReg()'.
#' @param which numeric; choose which plot to display.
#' '1' gives a plot of fit; '2' gives a plot of mixing probability.
#' @param xName character; Name used to pick x variable from data.
#' @param yName character; Name used to pick y variable from data.
#' @param xlab character; label that should be put on the x axis.
#' @param ylab character; label that should be put on the y axis.
#' @param ...	Further graphical parameters.
#'
#' @S3method plot mixtureReg
plot.mixtureReg <- function(mixtureModel, which = 1:2,
                            xName = NULL, yName = NULL,
                            xlab = NULL, ylab = NULL,
                            ...) {
  # plot method for "mixtureReg" class

  if (is.null(yName)) {yName = all.vars(mixtureModel$lmList[[1]]$terms)[1]}
  if (is.null(xName)) {xName = all.vars(mixtureModel$lmList[[1]]$terms)[2]}
  if (is.null(xlab)) {xlab = xName}
  if (is.null(ylab)) {ylab = yName}

  XX = mixtureModel$regData[ , xName]
  YY = mixtureModel$regData[ , yName]
  YhatList = lapply(X = mixtureModel$lmList, FUN = function(x) predict(x))

  if (which == 1) {
    plot(x = XX, y = YY, xlab = xlab, ylab = ylab, ...)
    for (i in 1:length(mixtureModel$lmList)) {
      orderedLines(x = XX, y = YhatList[[i]], col = i + 1)
    }
  }

  if (which == 2) {
    for (i in 1:length(mixtureModel$lmList)) {
      plot(x = XX, y = mixtureModel$posterior[[i]], xlab = xlab, ylab = paste0("Weights_", i), ...)
      orderedLines(x = XX, y = mixtureModel$prior[[i]], col = i + 1)
    }
  }
}

#' Obtain Log-likelihood from a mixtureReg Object
#'
#' S3 method for class 'mixtureReg'.
#' However, it doesn't return a 'logLik' object.
#' For simlicity, it returns a 'numeric' value.
#'
#' @param mixtureModel mixtureReg object, typically result from 'mixtureReg()'.
#' @return Return a numeric value of log likelihood.
#'
#' @S3method logLik mixtureReg
logLik.mixtureReg <- function(mixtureModel) {
  return(mixtureModel$"logLik")
}