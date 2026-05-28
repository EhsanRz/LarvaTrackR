#' Select a rectangular region of interest from a video frame
#'
#' Displays one frame from a list of video frames and allows the user to
#' interactively select a rectangular region of interest (ROI) by clicking two
#' opposite corners. The function returns the ROI coordinates as a list.
#'
#' @param frames A list of image frames, usually loaded from a video.
#' @param frame_number Numeric. The frame number to display for ROI selection.
#'   Default is `1`.
#'
#' @return A list containing `x_min`, `x_max`, `y_min`, and `y_max` coordinates
#'   of the selected ROI.
#'
#' @examples
#' \dontrun{
#' roi <- select_roi(frames, frame_number = 1)
#' }
#'
#' @importFrom EBImage channel normalize imageData
#' @importFrom graphics plot rasterImage locator rect
#' @export
select_roi <- function(frames,
                       frame_number = 1) {

  img <- frames[[frame_number]]

  if (length(dim(img)) == 3) {
    img <- EBImage::channel(img, "gray")
  }

  img <- EBImage::normalize(img)
  img_matrix <- EBImage::imageData(img)

  if (length(dim(img_matrix)) == 3) {
    img_matrix <- img_matrix[, , 1]
  }

  w <- dim(img_matrix)[1]
  h <- dim(img_matrix)[2]

  img_raster <- as.raster(t(img_matrix)[h:1, ])

  plot(
    NA,
    xlim = c(1, w),
    ylim = c(h, 1),
    asp = 1,
    xlab = "X",
    ylab = "Y",
    main = "Click TWO corners of ROI: top-left and bottom-right"
  )

  rasterImage(img_raster, 1, h, w, 1)

  clicked <- locator(n = 2, type = "p", pch = 16, col = "red", cex = 1.5)

  roi <- list(
    x_min = round(min(clicked$x)),
    x_max = round(max(clicked$x)),
    y_min = round(min(clicked$y)),
    y_max = round(max(clicked$y))
  )

  rect(
    roi$x_min,
    roi$y_min,
    roi$x_max,
    roi$y_max,
    border = "yellow",
    lwd = 2
  )

  return(roi)
}
