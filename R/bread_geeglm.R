#------------------------------------------------------------------------------#
#' geeglm bread matrices
#'
#' @param x `geeglm` object

#' @importFrom sandwich bread
#' @export
#------------------------------------------------------------------------------#

bread.geeglm <- function(x, ...)
{
  xmat   <- model.matrix(x)
  w      <- x$weights
  clust  <- x$id
  family <- x$family$family
  link   <- x$family$link
  family_link <- paste(family, link, sep = '_')

  if(x$corstr != 'independence'){
    stop("only independence working correlation is supported at this time")
  }

  m <- data.frame(w = w, xmat)

  if(family_link == 'gaussian_identity'){

    psi_prime <- lapply(split(m, clust), FUN = function(x){
      x <- as.matrix(x)
      X <- x[ , 2:ncol(x)]
      W <- diag(x[ ,1])
      eqns <- t(X) %*% solve(W) %*% X
      t(eqns)
    })

  } else {
    stop("only gaussian identity currently supported")
  }

  psi_prime <- array(unlist(psi_prime),
                     dim = c(nrow(psi_prime[[1]]),
                             ncol(psi_prime[[1]]),
                             length(psi_prime) ) )
  return( -apply(psi_prime, 1:2, mean) )
}
