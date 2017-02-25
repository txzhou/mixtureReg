# methods for mixtureReg
orderedLines <- function(x, y, ...) {
  xOrder <- order(x)
  lines(x = x[xOrder], y = y[xOrder], ...)
}

plot.2lm <- function(lmList, yName, xName, which = 1:2) {
  #lmList: a list of "lm" objects.
  if (xName %in% names(lmList[[1]]$model)) {
    XX = lmList[[1]]$model[ , xName]
  } else {
    XX = lmList[[2]]$model[ , xName]
  }
  YY = lmList[[2]]$model[ , yName]
  Yhat1 = predict(lmList[[1]])
  Yhat2 = predict(lmList[[2]])
  W1 = lmList[[1]]$weights
  W2 = lmList[[2]]$weights
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

plot.mixtureReg <- function(mixtureModel, yName = NULL, xName = NULL, which = 1:2) {
  if (is.null(yName)) {yName = names(mixtureModel$lmList[[1]]$model)[1]}
  if (is.null(xName)) {xName = names(mixtureModel$lmList[[1]]$model)[2]}
  plot.2lm(mixtureModel$lmList, yName, xName, which)
}
