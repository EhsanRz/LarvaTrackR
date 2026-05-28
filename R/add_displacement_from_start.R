#' Add displacement from the starting position
#'
#' Calculates the displacement of each tracked object from its initial
#' x and y position. Displacement is returned in both pixels and millimetres.
#'
#' @param tracks_movement A data frame containing tracking data.
#' @param x_col Name of the x-coordinate column. Default is "x".
#' @param y_col Name of the y-coordinate column. Default is "y".
#' @param id_col Name of the track ID column. Default is "track_id".
#' @param frame_col Name of the frame column. Default is "frame".
#' @param pixel_size_mm Pixel size in millimetres. Default is 10 / 54.
#'
#' @return A data frame with added columns for starting x and y positions,
#' displacement from start in pixels, and displacement from start in millimetres.
#'
#' @importFrom dplyr arrange group_by mutate ungroup first
#' @importFrom rlang .data
#'
#' @export
add_displacement_from_start <- function(tracks_movement,
                                        x_col = "x",
                                        y_col = "y",
                                        id_col = "track_id",
                                        frame_col = "frame",
                                        pixel_size_mm = 10 / 54) {


  tracks_movement %>%
    arrange(.data[[id_col]], .data[[frame_col]]) %>%
    group_by(.data[[id_col]]) %>%
    mutate(
      x_start = first(.data[[x_col]]),
      y_start = first(.data[[y_col]]),
      displacement_from_start_px = sqrt(
        (.data[[x_col]] - x_start)^2 +
          (.data[[y_col]] - y_start)^2
      ),
      displacement_from_start_mm = displacement_from_start_px * pixel_size_mm
    ) %>%
    ungroup()
}
