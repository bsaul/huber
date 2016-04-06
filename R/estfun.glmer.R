#------------------------------------------------------------------------------#
#' glmer Estimating Equations
#'
#' Currently supports Logistic-Normal model with univariate random effect.
#' @param grad.method method passed to \code{\link[numDeriv]{grad}}
#' @param grad.method.args method.args passed to \code{\link[numDeriv]{grad}}
#' @importFrom sandwich estfun
#' @export
#------------------------------------------------------------------------------#

estfun.glmerMod <- function(x, grad.method, grad.method.args = list(), ...)
{
  xmat   <- lme4::getME(x, 'X')
  resp   <- lme4::getME(x, 'y')
  parms  <- unlist(lme4::getME(x, c('beta', 'theta')))
  clust  <- lme4::getME(x, 'flist')
  family <- x@resp$family$family
  # objective.fun <- objFun.glmerMod(family)
  link_obj <- stats::make.link(stats::family(x)$link)
  gen_objFun_args <- list(
    parms = parms,
    response = resp,
    xmatrix = xmat,
    inv_link_fun = link_obj$linkinv
  )
  objective.fun <- do.call(objFun.glmerMod.generalized, args = gen_objFun_args)

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

#------------------------------------------------------------------------------#
#' glmer Objective Fundtion
#'
#'@param family
#'@param ... additional arguments pass to objective function
#'@export
#------------------------------------------------------------------------------#

objFun.glmerMod <- function(family, ...){
  switch(family,
         binomial = objFun.glmerMod.binomial,
         stop('Objective function not defined'))
}

#------------------------------------------------------------------------------#
#' glmer Objective Function for Logistic-Normal Likelihood
#'
#'@param vector of parameters
#'@param response
#'@param xmatrix
#'@export
#------------------------------------------------------------------------------#

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


#------------------------------------------------------------------------------#
#' glmer Objective Function for Likelihoods specified by glmerMod object
#'
#'@param vector of parameters
#'@param response
#'@param xmatrix
#'@param inv_link_fun
#'@export
#'#------------------------------------------------------------------------------#
#'
objFun.glmerMod.generalized <- function(parms, response, xmatrix, inv_link_fun)
{
  # Logistic-Normal Model
  integrand <- function(b, response, xmatrix, parms){
    lc <- outer(xmatrix %*% parms[-length(parms)], b, '+')
    h  <- apply(lc, 3, function(x) dbinom(response, 1, plogis(x) ) )
    hh <- apply(h, 2, prod)
    hh * dnorm(b, mean = 0, sd = parms[length(parms)])
  }

  inv_link_fun(integrate(integrand, lower = -Inf, upper = Inf, parms = parms,
                response = response, xmatrix = xmatrix)$value)

}


