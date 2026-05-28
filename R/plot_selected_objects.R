#' Plot selected objects on a video frame
#'
#' Displays a selected video frame and overlays manually selected object
#' positions. The function can show all selected objects or only the objects
#' selected for the displayed frame.
#'
#' @param frames A list of image frames, usually loaded from a video.
#' @param selected_objects A data frame containing selected object coordinates.
#'   It should contain `x` and `y` columns. Optional columns include `frame` and
#'   `selected_id`.
#' @param frame_number Numeric. The frame number to display. Default is `1`.
#' @param point_size Numeric. Size of plotted points. Default is `2`.
#' @param show_all_selected Logical. If `TRUE`, all selected objects are shown.
#'   If `FALSE`, only objects with `frame == frame_number` are shown.
#' @param flip_y Logical. If `TRUE`, flips the y coordinates for plotting.
#'   Default is `TRUE`.
#'
#' @return Invisibly returns `NULL`. This function is used for plotting.
#'
#' @examples
#' \dontrun{
#' plot_selected_objects(
#'   frames,
#'   selected_objects,
#'   frame_number = 1
#' )
#' }
#'
#' @importFrom EBImage channel normalize imageData
#' @importFrom graphics plot rasterImage points text
#' @export
plot_selected_objects <- function(frames,
                                  selected_objects,
                                  frame_number = 1,
                                  point_size = 2,
                                  show_all_selected = TRUE,
                                  flip_y = TRUE) {

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

  if (show_all_selected) {
    selected_now <- selected_objects
  } else {
    selected_now <- selected_objects[selected_objects$frame == frame_number, , drop = FALSE]
  }

  plot(
    NA,
    xlim = c(1, w),
    ylim = c(h, 1),
    xaxs = "i",
    yaxs = "i",
    asp = NA,
    xlab = "X",
    ylab = "Y",
    main = paste0("Selected object(s) - frame ", frame_number)
  )

  rasterImage(
    img_raster,
    xleft = 1,
    ybottom = h,
    xright = w,
    ytop = 1
  )

  if (nrow(selected_now) > 0) {

    selected_now$x_plot <- selected_now$x
    selected_now$y_plot <- selected_now$y

    if (flip_y) {
      selected_now$y_plot <- h - selected_now$y + 1
    }

    points(
      selected_now$x_plot,
      selected_now$y_plot,
      pch = 21,
      bg = "red",
      col = "yellow",
      cex = point_size
    )

    label_col <- if ("selected_id" %in% colnames(selected_now)) {
      selected_now$selected_id
    } else {
      seq_len(nrow(selected_now))
    }

    text(
      selected_now$x_plot,
      selected_now$y_plot - 15,
      labels = label_col,
      col = "yellow",
      cex = 0.9
    )
  }
}
