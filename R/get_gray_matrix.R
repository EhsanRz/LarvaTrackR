#' Convert an image frame to a grayscale matrix
#'
#' Converts an EBImage frame to grayscale, normalises pixel intensity values,
#' and returns the image data as a two-dimensional matrix.
#'
#' @param frame An EBImage image object.
#'
#' @return A numeric matrix containing grayscale pixel intensity values.
#'
#' @keywords internal
#'
#' @importFrom EBImage channel normalize imageData
get_gray_matrix <- function(frame) {

  if (length(dim(frame)) == 3) {
    frame <- EBImage::channel(frame, "gray")
  }

  frame <- EBImage::normalize(frame)
  mat <- EBImage::imageData(frame)

  if (length(dim(mat)) == 3) {
    mat <- mat[, , 1]
  }

  mat
}
