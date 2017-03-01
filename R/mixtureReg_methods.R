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
  YhatList = lapply(X = mixtureModel$lmList, FUN = function(x) predict(x))
  WList = lapply(X = mixtureModel$lmList, FUN = function(x) x$weights)

  if (1 %in% which) {
    plot(x = XX, y = YY, xlab = xName, ylab = yName)
    for (i in 1:length(mixtureModel$lmList)) {
      orderedLines(x = XX, y = YhatList[[i]], col = i + 1)
    }
  }

  if (2 %in% which) {
    for (i in 1:length(mixtureModel$lmList)) {
      plot(x = XX, y = WList[[i]], xlab = xName, ylab = paste0("Weights_", i))
      orderedLines(x = XX, y = predict(loess(WList[[i]] ~ XX)), col = i + 1)
    }
  }
}
