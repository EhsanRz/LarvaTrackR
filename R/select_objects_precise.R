#' Select objects precisely from a video frame
#'
#' Displays a selected video frame and allows the user to manually select
#' objects by clicking roughly on each object, followed by a zoomed-in view for
#' more precise centre selection. The function returns the selected object
#' coordinates as a data frame.
#'
#' @param frames A list of image frames, usually loaded from a video.
#' @param frame_number Numeric. The frame number to use for object selection.
#'   Default is `1`.
#' @param n Integer or `NULL`. Number of objects to select. If `NULL`, object
#'   selection continues until the user stops clicking.
#' @param zoom_size Numeric. Half-width of the zoom window around the rough
#'   clicked position, in pixels. Default is `40`.
#' @param point_size Numeric. Size of plotted selected points. Default is `0.6`.
#' @param rotate_180 Logical. If `TRUE`, rotates the image by 180 degrees before
#'   selection. Default is `TRUE`.
#'
#' @return A data frame with columns `selected_id`, `frame`, `x`, and `y`.
#'
#' @examples
#' \dontrun{
#' selected_objects <- select_objects_precise(
#'   frames,
#'   frame_number = 1,
#'   n = 5
#' )
#' }
#'
#' @importFrom EBImage channel normalize imageData
#' @importFrom graphics image points text locator abline
#' @importFrom grDevices gray.colors
#' @export
select_objects_precise <- function(frames,
                                   frame_number = 1,
                                   n = NULL,
                                   zoom_size = 40,
                                   point_size = 0.6,
                                   rotate_180 = TRUE) {

  if (!requireNamespace("EBImage", quietly = TRUE)) {
    stop("Package 'EBImage' is required.")
  }

  img <- frames[[frame_number]]

  if (length(dim(img)) == 3) {
    img <- EBImage::channel(img, "gray")
  }

  img <- EBImage::normalize(img)
  img_matrix <- EBImage::imageData(img)

  if (length(dim(img_matrix)) == 3) {
    img_matrix <- img_matrix[, , 1]
  }

  img_matrix <- as.matrix(img_matrix)

  # Rotate image 180 degrees
  if (rotate_180) {
    img_matrix <- img_matrix[nrow(img_matrix):1, ncol(img_matrix):1]
  }

  w <- nrow(img_matrix)
  h <- ncol(img_matrix)

  selected <- data.frame(
    selected_id = integer(),
    frame = integer(),
    x = numeric(),
    y = numeric()
  )

  plot_full_image <- function() {

    image(
      x = 1:w,
      y = 1:h,
      z = img_matrix,
      col = gray.colors(256),
      xlab = "X",
      ylab = "Y",
      main = paste0(
        "Frame ", frame_number,
        " | Click roughly on object"
      ),
      asp = 1
    )

    if (nrow(selected) > 0) {
      points(
        selected$x,
        selected$y,
        pch = 3,
        col = "red",
        cex = point_size,
        lwd = 1
      )

      text(
        selected$x,
        selected$y,
        labels = selected$selected_id,
        col = "red",
        pos = 3,
        cex = 0.8
      )
    }
  }

  repeat {

    if (!is.null(n) && nrow(selected) >= n) {
      break
    }

    plot_full_image()

    rough <- locator(1)

    if (length(rough$x) == 0) {
      break
    }

    x0 <- round(rough$x)
    y0 <- round(rough$y)

    x_min <- max(1, x0 - zoom_size)
    x_max <- min(w, x0 + zoom_size)
    y_min <- max(1, y0 - zoom_size)
    y_max <- min(h, y0 + zoom_size)

    image(
      x = x_min:x_max,
      y = y_min:y_max,
      z = img_matrix[x_min:x_max, y_min:y_max, drop = FALSE],
      col = gray.colors(256),
      xlab = "X",
      ylab = "Y",
      main = "Zoom view: click precise centre",
      asp = 1
    )

    abline(v = x0, col = "cyan", lwd = 1)
    abline(h = y0, col = "cyan", lwd = 1)

    precise <- locator(1)

    if (length(precise$x) == 0) {
      final_x <- rough$x
      final_y <- rough$y
    } else {
      final_x <- precise$x
      final_y <- precise$y
    }

    new_point <- data.frame(
      selected_id = nrow(selected) + 1,
      frame = frame_number,
      x = final_x,
      y = final_y
    )

    selected <- rbind(selected, new_point)

    message(
      "Selected object ", nrow(selected),
      ": x = ", round(final_x, 2),
      ", y = ", round(final_y, 2)
    )
  }

  plot_full_image()

  message(nrow(selected), " object(s) selected.")

  selected
}
