#------------------------------------------------------------------------------#
#' Estimating Equations for clustered data
#'
#' @param x \code{lm} or \code{glm} object
#' @param groups vector of groupings
#' @export
#------------------------------------------------------------------------------#
estfun.glm_cluster <- function(x, groups, ...)
{
#   if(class(x) != 'lm' | class(x) != 'glm'){
#     stop('Model must be of class lm or glm')
#   }
  out <- apply(estfun(x), 2, function(x) tapply(x, x$data[[groups]], sum))
  return(out)
}
