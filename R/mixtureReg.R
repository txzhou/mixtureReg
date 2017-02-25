mixtureReg <- function(regData, formulaList, initialWList = NULL,
                       epsilon = 1e-08, max_iter = 10000,
                       min_lambda = 0.05, min_sigmaRatio = 0.1, max_restart = 15,
                       silently = FALSE
) {
  # regData: data frame used in fitting model.
  # formulaList: a list of the regression components that need to be estimated.
  # initialWList: a list of weights guesses (provided by user).

  # missing values in data or data with no variance will crash the algorithm
  yName = all.vars(formulaList[[1]])[1]
  stopifnot(
    all(!is.na(regData[, yName])),
    var(regData[ ,yName]) > 0
    )

  require(dplyr)

  # E step
  conditionalP <- function(res, lambda, sigma) {
    lambda * dnorm(x = res, mean = 0, sd = sigma)
  }

  EUpdate <- function(result) {
    lmList = result$lmList
    lambdaList = result$lambdaList
    resList = lapply(lmList, function(m) m$residuals)
    sigmaList = mapply(function(m, lambda) {summary(m)$sigma / sqrt(lambda)}, lmList, lambdaList, SIMPLIFY = FALSE)
    PList = mapply(conditionalP, resList, lambdaList, sigmaList, SIMPLIFY = FALSE)
    sumsP = rowSums(do.call(cbind, PList))
    WList = lapply(PList, function(p) p/sumsP)
    llList = lapply(lmList, logLik)
    ll = sum(log(sumsP)) # Log-likelihood
    return(list(WList = WList, ll = ll))
  }

  # M step
  MUpdate <- function(WList) {
    lambdaList = sapply(WList, mean) %>%
      (function(x) (x > min_lambda)*x + (x <= min_lambda)*min_lambda) %>%
      (function(x) x/sum(x)) %>%
      as.list
    wlm <- function(i) {
      tempData <- regData
      tempData$WW <- WList[[i]]
      reg = lm(formula = formulaList[[i]], weights = WW, data = tempData)
      return(reg)
    }
    lmList = lapply(FUN = wlm, as.list(1:length(formulaList)))
    return(list(lmList = lmList, lambdaList = lambdaList))
  }

  # restart step
  isSingular <- function(lambdaList) {
    return(any(lambdaList < epsilon))
  }

  needRestart <- function(newResult, newLL){
    if (isSingular(newResult$lambdaList)) {
      errorMessage <- "sigular lambda list"
      answer <- TRUE
    } else {
      if (is.na(newLL) || newLL < ll || abs(newLL) == Inf) {
        errorMessage <- "abnormal logLik"
        answer <- TRUE
      } else {
        sigma_s = mapply(function(m, lambda) {summary(m)$sigma / sqrt(lambda)}, newResult$lmList, newResult$lambdaList, SIMPLIFY = TRUE)
        if (any(is.na(sigma_s))) {
          errorMessage <- "sigma is NA"
          answer <- TRUE
        } else {
          sigmaRatio_s = sigma_s/sigma_s[1]
          if (any(sigmaRatio_s < min_sigmaRatio) || any(sigmaRatio_s > 1/min_sigmaRatio)) {
            errorMessage <- "sigma ratio constraint"
            answer <- TRUE
          } else {
            errorMessage <- NA
            answer <- FALSE
          }
        }
      }
    }
    return(list("error_message" = errorMessage, "answer" = answer))
  }

  randomWList <- function(n, k) {
    randomWeightMatrix = matrix(runif(n*k, min = 0.1, max = 0.9), nrow = n, ncol = k)
    WMatrix = randomWeightMatrix / (rowSums(randomWeightMatrix))
    WList = lapply(as.list(1:k), function(i) WMatrix[,i])
    return(WList)
  }

  # Main routine
  {
    # initialize
    {
      diff <- 1
      iter <- 0
      restart <- 0

      n = dim(regData)[1]
      k = length(formulaList)

      # initialize (E step)
      if (!is.null(initialWList)) { # get WList from initial guess
        WList = initialWList
      } else {
        WList = randomWList(n, k)
      }

      # first M step
      newResult = MUpdate(WList)
      # second E step
      newE = EUpdate(newResult)
      # update
      result <- newResult
      WList <- newE$WList
      ll <- newE$ll

      monitor <- data.frame("diff" = diff, "iter" = iter, "restart" = restart, "logLik" = ll,
                            "newLL" = NA, "sigma1" = summary(newResult$lmList[[1]])$sigma, "sigma2" = summary(newResult$lmList[[2]])$sigma, "ratio" = summary(newResult$lmList[[1]])$sigma/summary(newResult$lmList[[2]])$sigma, "lambda1" = newResult$lambdaList[[1]], "lambda2" = newResult$lambdaList[[2]],
                            "error_message" = NA)
    }

    # while loop
    while (abs(diff) > epsilon && iter < max_iter && restart < max_restart) { # while not convegent
      # do an M step
      # do an E step
      # if sigular or no improvement, restart;
      # else update result and ll, and do another iteration.
      newResult = MUpdate(WList)
      newE = EUpdate(newResult)
      newLL = newE$ll

      judge <- needRestart(newResult, newLL)
      if (iter > 10 & judge$"answer") {
        WList = randomWList(n, k)
        restart <- restart + 1
      } else {
        diff = newLL - ll
        result <- newResult
        WList <- newE$WList
        ll <- newLL
      }
      iter <- iter + 1
      monitor <-
        rbind(
          monitor,
          c(diff, iter, restart, ll,
            newLL, summary(newResult$lmList[[1]])$sigma, summary(newResult$lmList[[2]])$sigma, summary(newResult$lmList[[1]])$sigma/summary(newResult$lmList[[2]])$sigma, newResult$lambdaList[[1]], newResult$lambdaList[[2]],
            judge$"error_message")
          )
    }

    if (!silently) {
      cat("diff = ", diff, "\n")
      cat("iter = ", iter, "\n")
      cat("restart = ", restart, "\n")
      cat("log-likelihood = ", ll, "\n")
    }

    mixtureRegModel = list(
      "lmList" = result$lmList,
      "monitor" = monitor,
      "regData" = regData)
    class(mixtureRegModel) = c("mixtureReg", class(mixtureRegModel))
    return(mixtureRegModel)
  }
}


initializeWList <- function(y, initialGuess,
                            lambdaList = list(0.1, 0.9), sigmaList = NULL) {
  # initialGuess: a list of guess

  if (is.null(sigmaList)) {
    ySigma = sd(y)
    sigmaList = list(ySigma, ySigma)
  }

  conditionalP <- function(res, lambda, sigma) {
    lambda * dnorm(x = res, mean = 0, sd = sigma)
  }

  resList = lapply(initialGuess, function(g) y-g)
  PList = mapply(conditionalP, resList, lambdaList, sigmaList, SIMPLIFY = FALSE)
  sumsP = rowSums(do.call(cbind, PList))

  WList = lapply(PList, function(p) {p/sumsP})

  return(WList)
}
