#------------------------------------------------------------------------------#
#' glmer Estimating Equations
#'
#' Currently supports Logistic-Normal model with univariate random effect.
#' @param grad.method method passed to \code{\link[numDeriv]{grad}}
#' @param grad.method.args method.args passed to \code{\link[numDeriv]{grad}}
#' @importFrom sandwich estfun
#' @export
#------------------------------------------------------------------------------#

estfun.glmerMod <- function(x, grad_method = 'simple', grad_options = NULL, ...)
{
  psis <- psi.glmerMod(x = x, grad_method = grad_method,
               grad_options = grad_options, deriv = FALSE, ...)
  return(do.call(rbind, psis))
}


#------------------------------------------------------------------------------#
#' glmer Estimating Equations
#'
#' Currently supports Logistic-Normal model with univariate random effect.
#' @param x `glmer` object
#' @param grad_method method passed to \code{\link[numDeriv]{grad}} or
#' \code{\link[numDeriv]{hessian}} when `hessian = TRUE`
#' @param grad_options method.args passed to \code{\link[numDeriv]{grad}} or
#'  \code{\link[numDeriv]{hessian}} when `hessian = TRUE`
#' @param deriv logical indicating whether to create derivative matrix of psi
#' @export
#------------------------------------------------------------------------------#

psi.glmerMod <- function(x, grad_method, grad_options = NULL, deriv, ...)
{
  xmat   <- lme4::getME(x, 'X')
  resp   <- lme4::getME(x, 'y')
  frame  <- as.data.frame(cbind(resp, xmat))
  parms  <- unlist(lme4::getME(x, c('beta', 'theta')))
  clust  <- lme4::getME(x, 'flist')
  family <- x@resp$family
  linkinv <- family$linkinv
  objective_fun <- objFun_glmerMod(family$family)

  deriv_fun <- if(deriv == TRUE) numDeriv::hessian else numDeriv::grad

  ## Warnings ##
  if(length(lme4::getME(x, 'theta')) > 1){
    stop('estfun.glmer currently does not handle >1 random effect')
  }

  split_data <- split(frame, clust)

  out <- mapply(split_data, SIMPLIFY = FALSE,
                FUN = function(group_data){
    group_data <- as.matrix(group_data)

    deriv_args <- list(objective_fun,
                       x = parms,
                       response = group_data[ , 1],
                       xmatrix  = group_data[ , -1],
                       linkinv  = linkinv,
                       method = grad_method,
                       method.args = grad_options)

    do.call(deriv_fun, args = deriv_args)
  })

  return(out)
}

#------------------------------------------------------------------------------#
#' glmer Objective Fundtion
#'
#'@param family
#'@param ... additional arguments pass to objective function
#'@export
#------------------------------------------------------------------------------#

objFun_glmerMod <- function(family, ...){
  switch(family,
         binomial = objFun_glmerMod_binomial,
         stop('Objective function not defined'))
}

#------------------------------------------------------------------------------#
#' glmer Objective Function for Logistic-Normal Likelihood
#'
#' @param parms vector of parameters
#' @param response
#' @param xmatrix
#' @param linkinv inverse link function
#' @export
#------------------------------------------------------------------------------#

objFun_glmerMod_binomial <- function(parms, response, xmatrix, linkinv)
{
  # Logistic-Normal Model
  integrand <- function(b, response, xmatrix, parms){
    pr_response <- linkinv( drop(outer(xmatrix %*% parms[-length(parms)], b, '+') ) )
    hh <- dbinom(response, 1, prob = pr_response )
    hha <- apply(hh, 2, prod)
    hha * dnorm(b, mean = 0, sd = parms[length(parms)])
  }

  log(integrate(integrand, lower = -Inf, upper = Inf, parms = parms,
                  response = response, xmatrix = xmatrix)$value)

}


# #------------------------------------------------------------------------------#
# #' glmer Objective Function for Likelihoods specified by glmerMod object
# #'
# #'@param vector of parameters
# #'@param response
# #'@param xmatrix
# #'@param inv_link_fun
# #'@export
# #------------------------------------------------------------------------------#
#
# objFun.glmerMod.generalized <- function(parms, response, xmatrix, inv_link_fun)
# {
#   # Logistic-Normal Model
#   integrand <- function(b, response, xmatrix, parms){
#     lp <- outer(xmatrix %*% parms[-length(parms)], b, '+')
#     h  <- apply(lp, 3, function(x) dbinom(response, 1, plogis(x) ) )
#     hh <- apply(h, 2, prod)
#     hh * dnorm(b, mean = 0, sd = parms[length(parms)])
#   }
#
#   inv_link_fun(integrate(integrand, lower = -Inf, upper = Inf, parms = parms,
#                 response = response, xmatrix = xmatrix)$value)
#
# }
