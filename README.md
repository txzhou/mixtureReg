# mixtureReg
An R package to fit mixture of linear regressions.

![An example] (./mx1.png)

## Summary
This package implements and improves an EM algorithm, which can obtain the MLE estimators when the goal is to fit two or more linear regressions through data.

Note that the word "linear" sounds restricting but when one feed in nonlinear transformations of predictors into it, one can fit nonlinear models as well.
This is not a big news for experienced users of linear regression.

## Installation
To install, use the `devtools` package.

```R
install.packages("devtools")
library(devtools)
devtools::install_github("txzhou/mixtureReg")
```

## Quick Start
A [guide] (./Guide.pdf) to use the `mixtureReg` package is provided.

## Why a new package
The already available function `regmixEM` in the `mixtools` package can complete a similar job but does not offer the option to impose restrictions to the coefficients.
This causes trouble for researchers who need more flexibility in modeling.

When the situation arises, it might be possible to do some clever data transformation so that to alter the model in order to use the current tools.
However, there is a way to solve it more intuitively.

In its `lm` class, R has already offered powerful coefficient restriction capability through the unique formula representing language.
So this package implements the algorithm based on the powerfulness of the `lm` class and now offers the same flexibility to model the mixture of regressions.

## Future plan

  - Create a `logLik` method: Which is a commonly seen method in R, and can potentially simplify the code.

## References
de Veaux RD (1989). "Mixtures of Linear Regressions." Computational Statistics and Data Analysis, 8, 227-245.

Tatiana Benaglia, Didier Chauveau, David R. Hunter, Derek Young (2009). mixtools: An R Package for Analyzing
  Finite Mixture Models. Journal of Statistical Software, 32(6), 1-29. URL http://www.jstatsoft.org/v32/i06/.
