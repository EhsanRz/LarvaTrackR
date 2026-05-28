#' Plot a single video frame
#'
#' Displays one frame from a list of image frames. If the frame has multiple
#' channels, it is converted to grayscale before plotting.
#'
#' @param frames A list of image frames, usually loaded from a video.
#' @param frame_number Numeric. The frame number to display. Default is `1`.
#'
#' @return Invisibly returns `NULL`. This function is used for plotting.
#'
#' @examples
#' \dontrun{
#' plot_frame(frames, frame_number = 1)
#' }
#'
#' @importFrom EBImage channel normalize imageData
#' @importFrom graphics plot rasterImage
#' @export
plot_frame <- function(frames, frame_number = 1) {

  img <- frames[[frame_number]]

  if (length(dim(img)) == 3) {
    img <- EBImage::channel(img, "gray")
  }

  img <- EBImage::normalize(img)
  mat <- EBImage::imageData(img)

  if (length(dim(mat)) == 3) {
    mat <- mat[, , 1]
  }

  w <- dim(mat)[1]
  h <- dim(mat)[2]

  img_raster <- as.raster(t(mat)[h:1, ])

  plot(
    NA,
    xlim = c(1, w),
    ylim = c(h, 1),
    asp = 1,
    xlab = "X",
    ylab = "Y",
    main = paste0("Frame ", frame_number)
  )

  rasterImage(img_raster, 1, h, w, 1)
}
