#' Plot simple object tracks
#'
#' Plots tracked object trajectories using x and y coordinates. Each track is
#' shown with a separate colour, with optional points, track labels, and equal
#' aspect ratio. The y-axis can be flipped for image-style display.
#'
#' @param tracks A data frame containing tracking data with x, y, track_id,
#' and frame columns.
#' @param frames Optional list of EBImage image frames. Required when
#' flip_y_for_display = TRUE.
#' @param frame_number Frame number used to determine image height when
#' flipping the y-axis.
#' @param show_points Logical. If TRUE, points are drawn on top of track lines.
#' @param line_width Width of the track lines.
#' @param point_size Size of the plotted points.
#' @param flip_y_for_display Logical. If TRUE, flips the y-axis to match
#' image display coordinates.
#' @param equal_aspect Logical. If TRUE, uses equal x and y scaling.
#'
#' @return A base R plot showing object tracks.
#'
#' @importFrom dplyr filter arrange
#' @importFrom EBImage channel imageData
#' @importFrom grDevices rainbow
#' @importFrom graphics plot lines points text legend
#'
#' @export
plot_track_simple <- function(tracks,
                              frames = NULL,
                              frame_number = 1,
                              show_points = TRUE,
                              line_width = 1,
                              point_size = 1.5,
                              flip_y_for_display = TRUE,
                              equal_aspect = TRUE) {

  plot_data <- tracks

  plot_data$x_plot <- plot_data$x

  if (flip_y_for_display) {

    if (!is.null(frames)) {
      img <- frames[[frame_number]]

      if (length(dim(img)) == 3) {
        img <- EBImage::channel(img, "gray")
      }

      mat <- EBImage::imageData(img)

      if (length(dim(mat)) == 3) {
        mat <- mat[, , 1]
      }

      h <- dim(mat)[2]

    } else {
      stop("Please provide frames when flip_y_for_display = TRUE, so image height can be used.")
    }

    plot_data$y_plot <- h - plot_data$y + 1

  } else {
    plot_data$y_plot <- plot_data$y
  }

  track_ids <- unique(plot_data$track_id)
  cols <- grDevices::rainbow(length(track_ids))
  names(cols) <- track_ids

  plot(
    NA,
    xlim = range(plot_data$x_plot, na.rm = TRUE),
    ylim = range(plot_data$y_plot, na.rm = TRUE),
    xlab = "X position",
    ylab = "Y position",
    main = "Simple track plot",
    asp = if (equal_aspect) 1 else NA
  )

  for (id in track_ids) {

    tr <- plot_data %>%
      dplyr::filter(track_id == id) %>%
      dplyr::arrange(frame)

    lines(
      tr$x_plot,
      tr$y_plot,
      col = cols[as.character(id)],
      lwd = line_width
    )

    if (show_points) {
      points(
        tr$x_plot,
        tr$y_plot,
        col = cols[as.character(id)],
        pch = 16,
        cex = point_size
      )
    }

    text(
      tr$x_plot[nrow(tr)],
      tr$y_plot[nrow(tr)] - 15,
      labels = id,
      col = cols[as.character(id)],
      cex = 0.9
    )
  }

  legend(
    "topright",
    legend = paste("Track", track_ids),
    col = cols,
    lwd = 2,
    pch = 16,
    bty = "n",
    cex = 0.8
  )
}
