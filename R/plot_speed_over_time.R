#' Plot speed over time
#'
#' Creates a speed-over-time plot from movement-tracking data. The function can
#' either show individual tracks or summarise speed across tracks at each time
#' point.
#'
#' @param tracks_movement A data frame containing movement data. It should
#'   contain `track_id`, `time_s`, and `speed_mm_s` columns.
#' @param individual Logical. If `TRUE`, plots each track separately. If
#'   `FALSE`, plots the mean speed across tracks. Default is `FALSE`.
#' @param show_sd Logical. If `TRUE`, adds a standard-deviation ribbon around
#'   the mean speed when `individual = FALSE`. Default is `TRUE`.
#' @param min_speed Numeric. Minimum speed to include. Default is `0`.
#' @param max_speed Numeric. Maximum speed to include. Default is `Inf`.
#'
#' @return A `ggplot` object showing speed over time.
#'
#' @examples
#' \dontrun{
#' plot_speed_over_time(tracks_movement)
#'
#' plot_speed_over_time(
#'   tracks_movement,
#'   individual = TRUE
#' )
#' }
#'
#' @importFrom dplyr filter group_by summarise n
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon theme_classic labs
#' @export
plot_speed_over_time <- function(tracks_movement,
                                 individual = FALSE,
                                 show_sd = TRUE,
                                 min_speed = 0,
                                 max_speed = Inf) {

  tracks_movement_clean <- tracks_movement %>%
    dplyr::filter(
      !is.na(speed_mm_s),
      speed_mm_s >= min_speed,
      speed_mm_s <= max_speed
    )

  if (individual) {

    ggplot2::ggplot(
      tracks_movement_clean,
      ggplot2::aes(
        x = time_s,
        y = speed_mm_s,
        colour = as.factor(track_id),
        group = track_id
      )
    ) +
      ggplot2::geom_line(linewidth = 0.8, alpha = 0.8) +
      ggplot2::theme_classic(base_size = 12) +
      ggplot2::labs(
        x = "Time (s)",
        y = "Speed (mm/s)",
        colour = "Track ID",
        title = "Individual speed over time"
      )

  } else {

    speed_df <- tracks_movement_clean %>%
      dplyr::group_by(time_s) %>%
      dplyr::summarise(
        mean_speed = mean(speed_mm_s, na.rm = TRUE),
        sd_speed = sd(speed_mm_s, na.rm = TRUE),
        n = dplyr::n(),
        .groups = "drop"
      )

    p <- ggplot2::ggplot(
      speed_df,
      ggplot2::aes(x = time_s, y = mean_speed)
    )

    if (show_sd) {
      p <- p +
        ggplot2::geom_ribbon(
          ggplot2::aes(
            ymin = mean_speed - sd_speed,
            ymax = mean_speed + sd_speed
          ),
          alpha = 0.15
        )
    }

    p +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::theme_classic(base_size = 12) +
      ggplot2::labs(
        x = "Time (s)",
        y = "Mean speed (mm/s)",
        title = "Mean speed over time"
      )
  }
}
