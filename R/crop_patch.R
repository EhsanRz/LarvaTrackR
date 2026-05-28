#' Crop a small image patch around a coordinate
#'
#' Crops a square patch from a matrix around a given x/y coordinate. The crop is
#' automatically limited to the matrix boundaries.
#'
#' @param mat A numeric matrix, usually a grayscale image matrix.
#' @param x Numeric. X coordinate of the patch centre.
#' @param y Numeric. Y coordinate of the patch centre.
#' @param radius Numeric. Radius of the patch in pixels. Default is `10`.
#'
#' @return A numeric matrix containing the cropped image patch.
#'
#' @keywords internal
crop_patch <- function(mat, x, y, radius = 10) {

  x <- round(x)
  y <- round(y)

  x_min <- max(1, x - radius)
  x_max <- min(nrow(mat), x + radius)
  y_min <- max(1, y - radius)
  y_max <- min(ncol(mat), y + radius)

  mat[x_min:x_max, y_min:y_max, drop = FALSE]
}
