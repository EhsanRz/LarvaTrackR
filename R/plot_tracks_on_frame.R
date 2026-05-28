#' Plot tracked paths on a video frame
#'
#' Displays a selected video frame and overlays tracked object paths. The
#' function can show the full track or only the track up to the displayed frame.
#'
#' @param tracks A data frame containing tracked positions. It should contain
#'   `track_id`, `frame`, `x`, and `y` columns.
#' @param frames A list of image frames, usually loaded from a video.
#' @param frame_number Numeric. The frame number to display. Default is `5`.
#' @param show_full_track Logical. If `TRUE`, the full track is shown. If
#'   `FALSE`, only positions up to `frame_number` are shown.
#' @param point_size Numeric. Size of plotted track points. Default is `1.5`.
#' @param line_width Numeric. Width of track lines. Default is `0.8`.
#' @param flip_y_for_display Logical. If `TRUE`, flips y coordinates for display.
#'   Default is `TRUE`.
#'
#' @return Invisibly returns `NULL`. This function is used for plotting.
#'
#' @examples
#' \dontrun{
#' plot_tracks_on_frame(
#'   tracks,
#'   frames,
#'   frame_number = 5
#' )
#' }
#'
#' @importFrom EBImage channel normalize imageData
#' @importFrom dplyr filter arrange
#' @importFrom graphics plot rasterImage lines points text legend
#' @importFrom grDevices rainbow
#' @export
plot_tracks_on_frame <- function(tracks,
                                 frames,
                                 frame_number = 5,
                                 show_full_track = TRUE,
                                 point_size = 1.5,
                                 line_width = 0.8,
                                 flip_y_for_display = TRUE) {

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
    xaxs = "i",
    yaxs = "i",
    asp = NA,
    xlab = "X position",
    ylab = "Y position",
    main = paste0("Tracked paths over frame ", frame_number)
  )

  rasterImage(
    img_raster,
    xleft = 1,
    ybottom = h,
    xright = w,
    ytop = 1
  )

  if (show_full_track) {
    plot_tracks_data <- tracks
  } else {
    plot_tracks_data <- tracks %>%
      dplyr::filter(frame <= frame_number)
  }

  plot_tracks_data <- plot_tracks_data %>%
    dplyr::mutate(
      x_plot = x,
      y_plot = if (flip_y_for_display) h - y + 1 else y
    )

  track_ids <- unique(plot_tracks_data$track_id)
  cols <- grDevices::rainbow(length(track_ids))
  names(cols) <- track_ids

  for (id in track_ids) {

    tr <- plot_tracks_data %>%
      dplyr::filter(track_id == id) %>%
      dplyr::arrange(frame)

    lines(
      tr$x_plot,
      tr$y_plot,
      col = cols[as.character(id)],
      lwd = line_width
    )

    points(
      tr$x_plot,
      tr$y_plot,
      col = cols[as.character(id)],
      pch = 16,
      cex = point_size
    )

    text(
      tr$x_plot[nrow(tr)],
      tr$y_plot[nrow(tr)] - 15,
      labels = id,
      col = cols[as.character(id)],
      cex = 1
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
