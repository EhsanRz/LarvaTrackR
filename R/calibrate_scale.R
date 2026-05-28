#' Calibrate pixel size from a known distance
#'
#' Displays one video frame and allows the user to click two points with a known
#' real-world distance. The function calculates the pixel size in millimetres
#' per pixel.
#'
#' @param frames A list of image frames, usually loaded from a video.
#' @param frame_number Numeric. The frame number to use for calibration.
#'   Default is `1`.
#' @param known_distance_mm Numeric. The real-world distance between the two
#'   clicked points, in millimetres.
#'
#' @return A numeric value representing pixel size in mm/pixel.
#'
#' @examples
#' \dontrun{
#' pixel_size_mm <- calibrate_scale(
#'   frames,
#'   frame_number = 1,
#'   known_distance_mm = 10
#' )
#' }
#'
#' @importFrom EBImage channel normalize imageData
#' @importFrom graphics plot rasterImage locator segments
#' @export
calibrate_scale <- function(frames,
                            frame_number = 1,
                            known_distance_mm) {

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
    main = paste0(
      "Click two points with known distance = ",
      known_distance_mm,
      " mm"
    )
  )

  rasterImage(img_raster, 1, h, w, 1)

  clicked <- locator(n = 2, type = "p", pch = 16, col = "red", cex = 1.5)

  dx <- clicked$x[2] - clicked$x[1]
  dy <- clicked$y[2] - clicked$y[1]

  distance_px <- sqrt(dx^2 + dy^2)
  pixel_size_mm <- known_distance_mm / distance_px

  segments(
    clicked$x[1], clicked$y[1],
    clicked$x[2], clicked$y[2],
    col = "yellow",
    lwd = 2
  )

  message("Known distance: ", known_distance_mm, " mm")
  message("Measured distance: ", round(distance_px, 2), " pixels")
  message("Pixel size: ", signif(pixel_size_mm, 4), " mm / pixel")

  return(pixel_size_mm)
}

