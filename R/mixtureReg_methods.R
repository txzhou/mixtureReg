# methods for mixtureReg
orderedLines <- function(x, y, ...) {
  # a helper function used in plotting
  xOrder <- order(x)
  lines(x = x[xOrder], y = y[xOrder], ...)
}

plot.mixtureReg <- function(mixtureModel, yName = NULL, xName = NULL, which = 1:2) {
  # plot method for "mixtureReg" class

  if (is.null(yName)) {yName = all.vars(mixtureModel$lmList[[1]]$terms)[1]}
  if (is.null(xName)) {xName = all.vars(mixtureModel$lmList[[1]]$terms)[2]}

  XX = mixtureModel$regData[ , xName]
  YY = mixtureModel$regData[ , yName]
  Yhat1 = predict(mixtureModel$lmList[[1]])
  Yhat2 = predict(mixtureModel$lmList[[2]])
  W1 = mixtureModel$lmList[[1]]$weights
  W2 = mixtureModel$lmList[[2]]$weights

  if (1 %in% which) {
    plot(x = XX, y = YY, xlab = xName, ylab = yName)
    orderedLines(x = XX, y = Yhat1, col = "red")
    orderedLines(x = XX, y = Yhat2, col = "red")
  }

  if (2 %in% which) {
    plot(x = XX, y = W2, xlab = xName, ylab = "Weights")
    orderedLines(x = XX, y = predict(loess(W2 ~ XX)), col = "red")
  }
}
