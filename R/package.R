#' LarvaTrackR
#'
#' Tools for manually selecting, tracking, visualising, and analysing larval
#' movement from video frames.
#'
#' @name LarvaTrackR
#' @keywords internal
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom grDevices as.raster
#' @importFrom stats dist sd
NULL

utils::globalVariables(c(
  "bout_length_frames",
  "displacement_from_start_mm",
  "displacement_from_start_px",
  "displacement_mm",
  "displacement_px",
  "dx_px",
  "dy_px",
  "frame",
  "freeze_group",
  "is_slow",
  "mean_distance",
  "mean_speed",
  "min_distance",
  "movement_mm",
  "movement_px_clean",
  "sd_mean_distance",
  "sd_speed",
  "speed_mm_s",
  "speed_mm_s_clean",
  "time_s",
  "total_distance_mm",
  "track_id",
  "x",
  "x_mm",
  "x_start",
  "y",
  "y_mm",
  "y_start"
))
