#------------------------------------------------------------------------------#
#' Stacks Estimating Equations from multiple models using same data
#'
#' @param models list of lists of models with following arguments: `formula`,
#' `method`, and `options`
#' @param data data.frame used in models
#' @export
#------------------------------------------------------------------------------#

estfun_stacker <- function(models, data)
{
  ee <- lapply(models, function(x){
    args <- append(x$options, list(formula = x$formula, data = data) )
    m <- do.call(x$method, args = args)
    return(estfun(m))
  })

  return(do.call(cbind, ee))
}
