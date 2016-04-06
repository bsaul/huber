#------------------------------------------------------------------------------#
#' geeglm Estimating Equations
#'
#' @param x \code{geeglm} object
#' @export
#------------------------------------------------------------------------------#

estfun.geeglm <- function(x, ...)
{
  xmat   <- model.matrix(x)
  r      <- x$residuals
  f      <- x$fitted.values
  w      <- x$weights
  lp     <- x$linear.predictor
  clust  <- x$id
  family <- x$family$family
  link   <- x$family$link
  family_link <- paste(family, link, sep = '_')
  psi    <- summary(x)$dispersion$Estimate

  if(x$corstr != 'independence'){
    stop("only independence working correlation is supported at this time")
  }

  m <- data.frame(f = f, r = r, w = w, lp = lp, xmat)
  out <- lapply(split(m, clust), FUN = function(x){
    x <- as.matrix(x)
    eqns <- gesteqn(xmat = x[, 5:ncol(x)], w = x[ ,'w'], f = x[ , 'f'],
                    r = x[ , 'r'], lp = x[, 'lp'], psi = psi,
                    family_link = family_link)
    t(eqns)
  })

  out <- matrix(unlist(out), ncol = ncol(xmat), byrow = T,
                dimnames = list(id = unique(clust),
                           variable = dimnames(xmat)[[2]]))

  return(out)
}

#------------------------------------------------------------------------------#
#' Formulas for Generalized Estimating Equations
#'
#' @param x the model matrix
#' @param w vector of weights
#' @param lc linear predictor (X * beta)
#' @param f fitted values
#' @param r residuals (Y - mu)
#' @param psi dispersion parameter
#' @param family model family
#------------------------------------------------------------------------------#

gesteqn <- function(xmat, w, lp, f, r, psi, family_link){
  switch(family_link,
         gaussian_identity = gesteqn.gaussian.identity(xmat, w, r),
         binomial_logit    = gesteqn.binomial.logit(xmat, w, lp, f, r, psi),
         "family and/or link not currently supported")
}

gesteqn.gaussian.identity <- function(xmat, w, r){
  t(xmat) %*% diag(w) %*% r
}

gesteqn.binomial.logit <- function(xmat, lp, w, f, r, psi){
  D <- apply(xmat, 2, function(x) x * exp(lc)/((1+exp(lp))^2) )
  # This only applies/works for independence working correlation matrices
  V <- psi * diag(f * (1 - f), ncol = length(f) )/length(f)
  t(D) %*% V %*% diag(w) %*% r
}
