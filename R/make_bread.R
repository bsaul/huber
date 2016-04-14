#------------------------------------------------------------------------------#
#' Creates a block diagonal bread matrix from multiple
#'
#' @param models list of lists of models with following arguments: `formula`,
#' `method`, and `options`
#' @export
#------------------------------------------------------------------------------#

make_bread <- function(models)
{
  Matrix::bdiag(lapply(models, function(model) bread(model)) )
}
