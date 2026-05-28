#' Plot displacement from starting position over time
#'
#' Creates a line plot showing how far each tracked object moves away from its
#' starting position over time.
#'
#' @param tracks_movement A data frame containing movement data. It should
#'   contain `track_id`, `time_s`, and `displacement_from_start_mm` columns.
#'
#' @return A `ggplot` object showing displacement from the starting position
#'   over time.
#'
#' @examples
#' \dontrun{
#' plot_displacement_from_start(tracks_movement)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_line theme_classic labs
#' @export
plot_displacement_from_start <- function(tracks_movement) {

  ggplot2::ggplot(
    tracks_movement,
    ggplot2::aes(
      x = time_s,
      y = displacement_from_start_mm,
      colour = as.factor(track_id)
    )
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::labs(
      x = "Time (s)",
      y = "Displacement from start (mm)",
      colour = "Track ID",
      title = "Displacement from starting position"
    )
}
