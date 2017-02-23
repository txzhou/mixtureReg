# mixtureReg
An R package to fit mixture of linear regressions.

## Summary
This package implements and improves an EM algorithm that was discribed in de Veaux RD (1989), which can obtain the MLE estimators when the goal is to fit two linear regressions through a data set.

Note that the word "linear" sounds restricting but when one feed in nonlinear transformations of predictors into it, one can fit nonlinear models as well.
This is not a big news for experienced users of linear regression.

## Why a new package
The already available function "regmixEM" in the "mixtools" package can complete a similar job but does not offer the option to impose restrictions to the coefficients.
This causes trouble for researchers who need more flexibility in modelling in some user cases.

When the situation arises, it might be possible to do some clever data transformation so that to alter the model in order to use the current tools.
However, there is a way to solve it more intuitively.

In its "lm" class, R has already offered powerful coefficient restriction capability through the unique formula representing language.
So this package implements the algorithm based on the powerfulness of the "lm" class and now offers the same flexibility to model the mixture of regressions.

## Example
Upcoming.

## Extension
Further developing ideas include:
  - Create a "logLik" method: Which is a commonly seen method in R, and can potentially simplify the code.
  - Extends to 3 or more regression lines: It is natural. However in practice it's not a common user case.

## Citation
de Veaux RD (1989). "Mixtures of Linear Regressions." Computational Statistics and Data Analysis, 8, 227-245.

Tatiana Benaglia, Didier Chauveau, David R. Hunter, Derek Young (2009). mixtools: An R Package for Analyzing
  Finite Mixture Models. Journal of Statistical Software, 32(6), 1-29. URL http://www.jstatsoft.org/v32/i06/.
