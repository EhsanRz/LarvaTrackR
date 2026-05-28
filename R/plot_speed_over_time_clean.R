#' Plot cleaned speed over time
#'
#' Creates a speed-over-time plot from cleaned movement-tracking data. The
#' function can either show individual tracks or summarise speed across tracks
#' at each time point.
#'
#' @param tracks_movement_clean A data frame containing cleaned movement data.
#'   It should contain `track_id`, `time_s`, and `speed_mm_s_clean` columns.
#' @param individual Logical. If `TRUE`, plots each track separately. If
#'   `FALSE`, plots the mean speed across tracks. Default is `TRUE`.
#' @param show_sd Logical. If `TRUE`, adds a standard-deviation ribbon around
#'   the mean speed when `individual = FALSE`. Default is `TRUE`.
#' @param max_speed Numeric. Maximum speed to include. Default is `Inf`.
#'
#' @return A `ggplot` object showing cleaned speed over time.
#'
#' @examples
#' \dontrun{
#' plot_speed_over_time_clean(tracks_movement_clean)
#'
#' plot_speed_over_time_clean(
#'   tracks_movement_clean,
#'   individual = FALSE
#' )
#' }
#'
#' @importFrom dplyr filter group_by summarise
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon theme_classic labs
#' @importFrom rlang .data
#' @export
plot_speed_over_time_clean <- function(tracks_movement_clean,
                                       individual = TRUE,
                                       show_sd = TRUE,
                                       max_speed = Inf) {

  tracks_movement_clean <- tracks_movement_clean %>%
    dplyr::filter(
      is.na(speed_mm_s_clean) | speed_mm_s_clean <= max_speed
    )

  if (individual) {

    ggplot2::ggplot(
      tracks_movement_clean,
      ggplot2::aes(
        x = time_s,
        y = speed_mm_s_clean,
        colour = as.factor(track_id),
        group = track_id
      )
    ) +
      ggplot2::geom_line(linewidth = 0.8, alpha = 0.8, na.rm = TRUE) +
      ggplot2::theme_classic(base_size = 12) +
      ggplot2::labs(
        x = "Time (s)",
        y = "Speed (mm/s)",
        colour = "Track ID",
        title = "Cleaned individual velocity over time"
      )

  } else {

    speed_df <- tracks_movement_clean %>%
      dplyr::group_by(time_s) %>%
      dplyr::summarise(
        mean_speed = mean(speed_mm_s_clean, na.rm = TRUE),
        sd_speed = sd(speed_mm_s_clean, na.rm = TRUE),
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
      ggplot2::geom_line(linewidth = 1, na.rm = TRUE) +
      ggplot2::theme_classic(base_size = 12) +
      ggplot2::labs(
        x = "Time (s)",
        y = "Mean speed (mm/s)",
        title = "Cleaned mean velocity over time"
      )
  }
}
