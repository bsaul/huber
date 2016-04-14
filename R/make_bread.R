#------------------------------------------------------------------------------#
#' Creates a block diagonal bread matrix from multiple models
#'
#' @param models list of lists of models with following arguments: `formula`,
#' `method`, and `options`
#' @export
#------------------------------------------------------------------------------#

make_bread <- function(models, grad_method = NULL)
{
  Matrix::bdiag(lapply(models, function(model) bread(model, grad_method = grad_method)) )
}
