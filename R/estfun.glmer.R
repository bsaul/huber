#' glmer Estimating Equations
#'
#'@param grad.method
#' @export

estfun.glmer <- function(x, grad.method, ...)
{
  xmat   <- lme4::getME(x, 'X')
  resp   <- lme4::getME(x, 'y')
  parms  <- unlist(lme4::getME(x, c('beta', 'theta')))
  nparms <- length(parms)
  clust  <- lme4::getME(x, 'flist')
  family <- x@resp$family$family
  objective.fun <- sandwichShop::objFun.glmer(family)

  if(length(lme4::getME(x, 'theta')) > 1){
    stop('Not yet sure how to handle > 1 random effect')
  }


  out <- by(cbind(resp, xmat), clust, simplify = F, FUN = function(x) {
    x <- as.matrix(x)
    numDeriv::grad(objective.fun, x = parms,
                   Y = x[, 1], X = x[, -1], method = grad.method)
  })

  do.call('rbind', out)
}


#' glmer Objective Fundtion
#'
#'@param family
#'@export
#'
objFun.glmer <- function(family){
  switch(family,
         binomial = objFun.glmer.binomial,
         stop('Objective function not defined'))
}

#' glmer Objective Function for Logistic-Normal Likelihood
#'
#'@param Y
#'@param X
#'@param parms
#'@export
#'

objFun.glmer.binomial <- function(Y, X, parms)
{
  # Logistic-Normal Model
  integrand <- function(b, Y, X, parms){
    lc <- outer(X %*% parms[-length(parms)], b, '+')
    h  <- apply(lc, 3, function(x) dbinom(Y, 1, plogis(x) ) )
    hh <- apply(h, 2, prod)
    hh * dnorm(b, mean = 0, sd = parms[length(parms)])
  }

  fout <- function(parms, Y, X){
    log(integrate(integrand, lower = -Inf, upper = Inf, parms = parms,
                  Y = Y, X = X)$value)
  }

  return(fout)
}



