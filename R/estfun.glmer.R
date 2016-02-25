#' glmer Estimating Equations
#'
#' Currently supports Logistic-Normal model with univariate random effect.
#' @param grad.method method passed to \code{\link[numDeriv]{grad}}
#' @param grad.method.args method.args passed to \code{\link[numDeriv]{grad}}
#' @importFrom sandwich estfun
#' @export

estfun.glmerMod <- function(x, grad.method, grad.method.args = list(), ...)
{
  xmat   <- lme4::getME(x, 'X')
  resp   <- lme4::getME(x, 'y')
  parms  <- unlist(lme4::getME(x, c('beta', 'theta')))
  clust  <- lme4::getME(x, 'flist')
  family <- x@resp$family$family

  # TODO: Use objFun function to switch between objective functions
  objective.fun <- objFun.glmerMod(family)

  if(length(lme4::getME(x, 'theta')) > 1){
    stop('Not yet sure how to handle > 1 random effect')
  }

  out <- by(cbind(resp, xmat), clust, simplify = F, FUN = function(x) {
    x <- as.matrix(x)
    numDeriv::grad(objective.fun, x = parms,
                   response = x[ , 1], xmatrix = x[ , -1],
                   method = grad.method, method.args = grad.method.args)
  })

  do.call('rbind', out)
}

#' glmer Objective Fundtion
#'
#'@param family
#'@export
#'
objFun.glmerMod <- function(family, ...){
  switch(family,
         binomial = objFun.glmerMod.binomial,
         stop('Objective function not defined'))
}

#' glmer Objective Function for Logistic-Normal Likelihood
#'
#'@param vector of parameters
#'@param response
#'@param xmatrix
#'@export

objFun.glmerMod.binomial <- function(parms, response, xmatrix)
{
  # Logistic-Normal Model
  integrand <- function(b, response, xmatrix, parms){
    lc <- outer(xmatrix %*% parms[-length(parms)], b, '+')
    h  <- apply(lc, 3, function(x) dbinom(response, 1, plogis(x) ) )
    hh <- apply(h, 2, prod)
    hh * dnorm(b, mean = 0, sd = parms[length(parms)])
  }

  log(integrate(integrand, lower = -Inf, upper = Inf, parms = parms,
                  response = response, xmatrix = xmatrix)$value)

}



