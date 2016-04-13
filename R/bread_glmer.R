#------------------------------------------------------------------------------#
#' glmer bread matrices
#'
#' Currently supports Logistic-Normal model with univariate random effect.
#' @param grad.method method passed to \code{\link[numDeriv]{grad}}
#' @param grad.method.args method.args passed to \code{\link[numDeriv]{grad}}
#' @importFrom sandwich estfun
#' @export
#------------------------------------------------------------------------------#

bread.glmerMod <- function(x, grad_options = NULL, ...)
{
  psi.glmerMod(x = x, grad_options = grad_options, hessian = TRUE, ...)
}


