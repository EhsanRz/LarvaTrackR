#' Add freezing status to movement tracks
#'
#' Identifies freezing periods based on a speed threshold and a minimum freezing
#' bout duration. A frame is classified as freezing only when the object remains
#' below the speed threshold for at least the specified duration.
#'
#' @param tracks_movement A data frame containing movement data. It should
#'   contain `track_id`, `frame`, and a speed column.
#' @param speed_threshold Numeric. Speed threshold below which an object is
#'   considered slow. Default is `0.2`.
#' @param min_freeze_duration_s Numeric. Minimum duration in seconds required
#'   to define a freezing bout. Default is `2`.
#' @param fps Numeric. Frames per second of the video. Default is `5`.
#' @param speed_col Character. Name of the speed column used for freezing
#'   detection. Default is `"speed_mm_s_clean"`.
#'
#' @return A data frame with additional columns `is_slow`, `freeze_group`,
#'   `bout_length_frames`, and `freezing`.
#'
#' @examples
#' \dontrun{
#' tracks_freezing <- add_freezing_status(
#'   tracks_movement,
#'   speed_threshold = 0.2,
#'   min_freeze_duration_s = 2,
#'   fps = 5,
#'   speed_col = "speed_mm_s"
#' )
#' }
#'
#' @importFrom dplyr arrange group_by mutate if_else lag first n ungroup
#' @importFrom rlang .data
#' @export
add_freezing_status <- function(tracks_movement,
                                speed_threshold = 0.2,
                                min_freeze_duration_s = 2,
                                fps = 5,
                                speed_col = "speed_mm_s_clean") {

  if (!speed_col %in% colnames(tracks_movement)) {
    stop("Column '", speed_col, "' not found in tracks_movement.")
  }

  min_freeze_frames <- ceiling(min_freeze_duration_s * fps)

  tracks_movement %>%
    dplyr::arrange(track_id, frame) %>%
    dplyr::group_by(track_id) %>%
    dplyr::mutate(
      is_slow = .data[[speed_col]] < speed_threshold,
      is_slow = dplyr::if_else(is.na(is_slow), FALSE, is_slow),
      freeze_group = cumsum(
        is_slow != dplyr::lag(is_slow, default = dplyr::first(is_slow))
      )
    ) %>%
    dplyr::group_by(track_id, freeze_group) %>%
    dplyr::mutate(
      bout_length_frames = dplyr::n(),
      freezing = is_slow & bout_length_frames >= min_freeze_frames
    ) %>%
    dplyr::ungroup()
}
