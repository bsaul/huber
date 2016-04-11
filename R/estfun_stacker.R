#------------------------------------------------------------------------------#
#' Stacks Estimating Equations from multiple models using same data
#'
#' @param models list of lists of models with following arguments: `formula`,
#' `method`, and `options`
#' @param data data.frame used in models
#' @export
#------------------------------------------------------------------------------#

estfun_stacker <- function(models)
{
  ee <- lapply(models, function(x) estfun(x))

  return(do.call(cbind, ee))
}
