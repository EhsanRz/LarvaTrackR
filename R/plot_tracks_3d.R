#' Plot tracked object paths in 3D over time
#'
#' Creates an interactive 3D plot of object tracks across video frames using
#' `plotly`. The first and last video frames are shown as image planes, and
#' tracked object positions are plotted through time.
#'
#' @param tracks A data frame containing tracked positions. It should contain
#'   `track_id`, `frame`, `x`, and `y` columns.
#' @param frames A list of image frames, usually loaded from a video.
#' @param frame_mats Optional list of precomputed grayscale image matrices. If
#'   `NULL`, frames are converted internally using `get_gray_matrix()`.
#' @param fps Numeric. Frames per second of the video. Default is `5`.
#' @param first_frame Numeric. First frame to include in the 3D plot. Default is
#'   `1`.
#' @param last_frame Numeric. Last frame to include in the 3D plot. Default is
#'   `length(frames)`.
#' @param image_downsample Numeric. Downsampling factor for displaying image
#'   planes. Higher values make the plot lighter and faster. Default is `4`.
#' @param flip_y_for_display Logical. If `TRUE`, flips y coordinates for display.
#'   Default is `TRUE`.
#'
#' @return A `plotly` object showing tracked paths in 3D.
#'
#' @examples
#' \dontrun{
#' plot_tracks_3d(
#'   tracks,
#'   frames,
#'   fps = 5
#' )
#' }
#'
#' @importFrom dplyr mutate filter arrange
#' @importFrom plotly plot_ly add_surface add_trace layout
#' @export
plot_tracks_3d <- function(tracks,
                           frames,
                           frame_mats = NULL,
                           fps = 5,
                           first_frame = 1,
                           last_frame = length(frames),
                           image_downsample = 4,
                           flip_y_for_display = TRUE) {

  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Please install plotly: install.packages('plotly')")
  }

  if (is.null(frame_mats)) {
    frame_mats <- lapply(frames, get_gray_matrix)
  }

  mat_first_full <- frame_mats[[first_frame]]
  mat_last_full  <- frame_mats[[last_frame]]

  w <- nrow(mat_first_full)
  h <- ncol(mat_first_full)

  tracks_plot <- tracks %>%
    dplyr::mutate(
      time_s = (frame - first_frame) / fps,
      x_plot = x,
      y_plot = if (flip_y_for_display) h - y + 1 else y
    )

  mat_first <- mat_first_full[
    seq(1, nrow(mat_first_full), by = image_downsample),
    seq(1, ncol(mat_first_full), by = image_downsample)
  ]

  mat_last <- mat_last_full[
    seq(1, nrow(mat_last_full), by = image_downsample),
    seq(1, ncol(mat_last_full), by = image_downsample)
  ]

  time_first <- 0
  time_last  <- (last_frame - first_frame) / fps

  x_pos_first <- seq(1, nrow(mat_first)) * image_downsample
  y_pos_first <- seq(1, ncol(mat_first)) * image_downsample

  x_pos_last <- seq(1, nrow(mat_last)) * image_downsample
  y_pos_last <- seq(1, ncol(mat_last)) * image_downsample

  plane_first_time <- matrix(
    time_first,
    nrow = length(x_pos_first),
    ncol = length(y_pos_first)
  )

  plane_first_x <- matrix(
    rep(x_pos_first, times = length(y_pos_first)),
    nrow = length(x_pos_first),
    ncol = length(y_pos_first)
  )

  plane_first_y <- matrix(
    rep(y_pos_first, each = length(x_pos_first)),
    nrow = length(x_pos_first),
    ncol = length(y_pos_first)
  )

  plane_last_time <- matrix(
    time_last,
    nrow = length(x_pos_last),
    ncol = length(y_pos_last)
  )

  plane_last_x <- matrix(
    rep(x_pos_last, times = length(y_pos_last)),
    nrow = length(x_pos_last),
    ncol = length(y_pos_last)
  )

  plane_last_y <- matrix(
    rep(y_pos_last, each = length(x_pos_last)),
    nrow = length(x_pos_last),
    ncol = length(y_pos_last)
  )

  p <- plotly::plot_ly()

  p <- p %>%
    plotly::add_surface(
      x = plane_first_time,
      y = plane_first_x,
      z = plane_first_y,
      surfacecolor = mat_first,
      colorscale = list(c(0, "black"), c(1, "white")),
      showscale = FALSE,
      opacity = 0.8,
      name = "First frame"
    )

  p <- p %>%
    plotly::add_surface(
      x = plane_last_time,
      y = plane_last_x,
      z = plane_last_y,
      surfacecolor = mat_last,
      colorscale = list(c(0, "black"), c(1, "white")),
      showscale = FALSE,
      opacity = 0.8,
      name = "Last frame"
    )

  track_ids <- unique(tracks_plot$track_id)

  for (id in track_ids) {

    tr <- tracks_plot %>%
      dplyr::filter(track_id == id) %>%
      dplyr::arrange(frame)

    p <- p %>%
      plotly::add_trace(
        data = tr,
        x = ~time_s,
        y = ~x_plot,
        z = ~y_plot,
        type = "scatter3d",
        mode = "lines+markers",
        line = list(width = 5),
        marker = list(size = 3),
        name = paste("Track", id)
      )
  }

  p <- p %>%
    plotly::layout(
      scene = list(
        xaxis = list(title = "Time (s)"),
        yaxis = list(title = "Position X"),
        zaxis = list(title = "Position Y"),
        aspectmode = "manual",
        aspectratio = list(x = 2.5, y = 1, z = 1)
      ),
      title = "3D object tracks over time"
    )

  return(p)
}
