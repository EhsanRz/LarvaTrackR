#' Calculate similarity score between two image patches
#'
#' Calculates the sum of squared differences between a template patch and a
#' candidate patch. Lower values indicate a better match. If the two patches do
#' not have the same dimensions, the function returns `Inf`.
#'
#' @param template A numeric matrix representing the template image patch.
#' @param candidate A numeric matrix representing the candidate image patch.
#'
#' @return A numeric similarity score. Lower scores indicate greater similarity.
#'   Returns `Inf` if the patch dimensions do not match.
#'
#' @keywords internal
patch_score <- function(template, candidate) {

  if (!all(dim(template) == dim(candidate))) {
    return(Inf)
  }

  sum((template - candidate)^2, na.rm = TRUE)
}
