#' Plot cumulative distance travelled over time
#'
#' Creates a line plot showing the cumulative distance travelled by each tracked
#' object over time.
#'
#' @param tracks_movement A data frame containing movement data. It should
#'   contain `track_id`, `time_s`, and `total_distance_mm` columns.
#'
#' @return A `ggplot` object showing cumulative distance over time.
#'
#' @examples
#' \dontrun{
#' plot_total_distance(tracks_movement)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_line theme_classic labs
#' @export
plot_total_distance <- function(tracks_movement) {

  ggplot2::ggplot(
    tracks_movement,
    ggplot2::aes(
      x = time_s,
      y = total_distance_mm,
      colour = as.factor(track_id)
    )
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::labs(
      x = "Time (s)",
      y = "Total distance travelled (mm)",
      colour = "Track ID",
      title = "Cumulative movement over time"
    )
}
