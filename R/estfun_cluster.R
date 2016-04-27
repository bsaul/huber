#------------------------------------------------------------------------------#
#' Estimating Equations for clustered data
#'
#' @param x \code{lm} or \code{glm} object
#' @param groups vector of groupings
#' @export
#------------------------------------------------------------------------------#
estfun_cluster <- function(x, clusters, ...)
{
  if(!any(class(x) %in% c('lm', 'glm'))){
    stop('Model must be of class lm or glm')
  }
  out <- apply(estfun(x), 2, function(x) tapply(x, x$data[[clusters]], sum))
  return(out)
}
