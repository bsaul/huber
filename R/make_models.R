#------------------------------------------------------------------------------#
#' Makes model objects from list of models
#'
#' @param models_args list of model arguments
#' @param data used for models
#' @export
#------------------------------------------------------------------------------#

make_models <- function(model_args, data)
{
  out <- lapply(model_args, function(x){
    method <- match.fun(x$method)
    if(is.null(x$user)) x$user <- FALSE

    if(x$user == TRUE){
      NULL # For now
    } else {
      args <- append(x$options, list(formula = x$formula, data = data) )
      do.call(method, args = args)
    }
  })
  return(out)
}
