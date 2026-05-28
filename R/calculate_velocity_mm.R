#' Calculate movement velocity in millimetres
#'
#' Calculates frame-to-frame displacement and speed for tracked objects using
#' the video frame rate and calibrated pixel size.
#'
#' @param tracks A data frame containing tracked positions. It should contain
#'   `track_id`, `frame`, `x`, and `y` columns.
#' @param fps Numeric. Frames per second of the video. Default is `5`.
#' @param pixel_size_mm Numeric. Pixel size in millimetres per pixel, usually
#'   calculated using `calibrate_scale()`.
#'
#' @return A data frame with additional movement columns, including `time_s`,
#'   `dx_px`, `dy_px`, `displacement_px`, `displacement_mm`, `speed_mm_s`,
#'   `x_mm`, and `y_mm`.
#'
#' @examples
#' \dontrun{
#' tracks_movement <- calculate_velocity_mm(
#'   tracks,
#'   fps = 5,
#'   pixel_size_mm = pixel_size_mm
#' )
#' }
#'
#' @importFrom dplyr arrange group_by mutate lag ungroup
#' @export

calculate_velocity_mm <- function(tracks, fps = 5, pixel_size_mm) {

  tracks %>%
    dplyr::arrange(track_id, frame) %>%
    dplyr::group_by(track_id) %>%
    dplyr::mutate(
      time_s = (frame - min(frame)) / fps,
      dx_px = x - dplyr::lag(x),
      dy_px = y - dplyr::lag(y),
      displacement_px = sqrt(dx_px^2 + dy_px^2),
      displacement_mm = displacement_px * pixel_size_mm,
      speed_mm_s = displacement_mm * fps,
      x_mm = x * pixel_size_mm,
      y_mm = y * pixel_size_mm
    ) %>%
    dplyr::ungroup()
}
