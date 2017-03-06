# methods for mixtureReg
orderedLines <- function(x, y, ...) {
  # a helper function used in plotting
  xOrder <- order(x)
  lines(x = x[xOrder], y = y[xOrder], ...)
}

plot.mixtureReg <- function(mixtureModel, which = 1:2,
                            yName = NULL, xName = NULL,
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

logLik.mixtureReg <- function(mixtureModel) {
  return(mixtureModel$"logLik")
}