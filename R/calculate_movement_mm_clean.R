#' Calculate cleaned movement metrics in millimetres
#'
#' Calculates frame-to-frame movement, speed, cumulative distance, and
#' displacement from the starting position. Small movements below a threshold
#' are treated as zero, large jumps above a threshold are treated as missing,
#' and early frames can be excluded from movement calculations.
#'
#' @param tracks A data frame containing tracked positions. It should contain
#'   `track_id`, `frame`, `x`, and `y` columns.
#' @param fps Numeric. Frames per second of the video. Default is `5`.
#' @param pixel_size_mm Numeric. Pixel size in millimetres per pixel, usually
#'   calculated using `calibrate_scale()`.
#' @param min_step_px Numeric. Minimum movement step in pixels. Movements below
#'   this value are treated as zero. Default is `1`.
#' @param max_step_px Numeric. Maximum allowed movement step in pixels.
#'   Movements above this value are treated as missing. Default is `25`.
#' @param remove_first_frames Numeric. Number of early frames to exclude from
#'   movement calculations. Default is `10`.
#'
#' @return A data frame with additional movement columns, including `time_s`,
#'   `x_mm`, `y_mm`, `dx_px`, `dy_px`, `movement_px_raw`,
#'   `movement_px_clean`, `movement_mm`, `speed_mm_s`,
#'   `total_distance_mm`, and `displacement_from_start_mm`.
#'
#' @examples
#' \dontrun{
#' tracks_movement <- calculate_movement_mm_clean(
#'   tracks,
#'   fps = 5,
#'   pixel_size_mm = pixel_size_mm,
#'   min_step_px = 1,
#'   max_step_px = 25,
#'   remove_first_frames = 10
#' )
#' }
#'
#' @importFrom dplyr arrange group_by mutate lag case_when if_else first ungroup
#' @export
calculate_movement_mm_clean <- function(tracks,
                                        fps = 5,
                                        pixel_size_mm,
                                        min_step_px = 1,
                                        max_step_px = 25,
                                        remove_first_frames = 10) {

  tracks %>%
    dplyr::arrange(track_id, frame) %>%
    dplyr::group_by(track_id) %>%
    dplyr::mutate(
      time_s = (frame - min(frame)) / fps,

      x_mm = x * pixel_size_mm,
      y_mm = y * pixel_size_mm,

      dx_px = x - dplyr::lag(x),
      dy_px = y - dplyr::lag(y),

      movement_px_raw = sqrt(dx_px^2 + dy_px^2),

      movement_px_clean = dplyr::case_when(
        frame <= remove_first_frames ~ NA_real_,
        movement_px_raw < min_step_px ~ 0,
        movement_px_raw > max_step_px ~ NA_real_,
        TRUE ~ movement_px_raw
      ),

      movement_mm = movement_px_clean * pixel_size_mm,
      speed_mm_s = movement_mm * fps,

      total_distance_mm = cumsum(
        dplyr::if_else(is.na(movement_mm), 0, movement_mm)
      ),

      displacement_from_start_mm = sqrt(
        (x_mm - dplyr::first(x_mm))^2 +
          (y_mm - dplyr::first(y_mm))^2
      )
    ) %>%
    dplyr::ungroup()
}
