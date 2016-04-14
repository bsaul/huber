#------------------------------------------------------------------------------#
#' glmer bread matrices
#'
#' Currently supports Logistic-Normal model with single random effect.
#' @param x `glmer` object
#' @param grad_method method passed to \code{\link[numDeriv]{hessian}}
#' @param grad_options method.args passed to \code{\link[numDeriv]{hessian}}
#' @importFrom sandwich bread
#' @export
#------------------------------------------------------------------------------#

bread.glmerMod <- function(x, grad_method, grad_options = NULL, ...)
{
  psi_prime <- psi.glmerMod(x = x, grad_method,
                           grad_options = grad_options,
                           deriv = TRUE, ...)
  psi_prime <- array(unlist(psi_prime),
                     dim = c(nrow(psi_prime[[1]]),
                             ncol(psi_prime[[1]]),
                             length(psi_prime) ) )
  return( -apply(psi_prime, 1:2, mean) )
}


