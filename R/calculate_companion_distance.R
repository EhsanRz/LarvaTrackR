#' Calculate distance to companion tracks
#'
#' Calculates the distance from each tracked object to other tracked objects
#' within the same frame. For each object, the function returns the minimum and
#' mean distance to its companions over time.
#'
#' @param tracks A data frame containing tracked positions. It should contain
#'   `track_id`, `frame`, `x`, and `y` columns.
#' @param fps Numeric. Frames per second of the video. Default is `5`.
#' @param pixel_size Numeric. Pixel size used to convert pixel distances into
#'   real-world units. Default is `1`, which keeps distances in pixels.
#'
#' @return A data frame with columns `frame`, `time_s`, `track_id`,
#'   `min_distance`, and `mean_distance`.
#'
#' @examples
#' \dontrun{
#' companion_distance <- calculate_companion_distance(
#'   tracks,
#'   fps = 5,
#'   pixel_size = pixel_size_mm
#' )
#' }
#'
#' @importFrom dplyr filter select bind_rows
#' @export
calculate_companion_distance <- function(tracks, fps = 5, pixel_size = 1) {

  frames <- sort(unique(tracks$frame))
  out <- list()

  for (f in frames) {

    df <- tracks %>%
      dplyr::filter(frame == f) %>%
      dplyr::select(track_id, x, y)

    if (nrow(df) < 2) next

    dist_mat <- as.matrix(dist(df[, c("x", "y")]))
    dist_mat <- dist_mat * pixel_size

    diag(dist_mat) <- NA

    frame_summary <- data.frame(
      frame = f,
      time_s = f / fps,
      track_id = df$track_id,
      min_distance = apply(dist_mat, 1, min, na.rm = TRUE),
      mean_distance = apply(dist_mat, 1, mean, na.rm = TRUE)
    )

    out[[as.character(f)]] <- frame_summary
  }

  dplyr::bind_rows(out)
}
