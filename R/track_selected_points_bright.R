#' Track bright selected objects across video frames
#'
#' Tracks manually selected bright objects across a sequence of video frames.
#' For each selected object, the function searches within a local radius around
#' the previous position, detects the brightest pixels using a quantile-based
#' threshold, and uses their centroid as the new object position.
#'
#' This method is useful when the tracked objects are brighter than the
#' surrounding background.
#'
#' @param frames A list of image frames, usually loaded from a video.
#' @param selected_objects A data frame of manually selected objects. It should
#'   contain `selected_id`, `frame`, `x`, and `y` columns.
#' @param frame_mats Optional list of precomputed grayscale image matrices. If
#'   `NULL`, frames are converted internally using `get_gray_matrix()`.
#' @param search_radius Numeric. Radius around the previous object position to
#'   search in the next frame, in pixels. Default is `35`.
#' @param threshold_quantile Numeric. Quantile used to define bright pixels
#'   within the local search region. Default is `0.97`.
#' @param min_pixels Integer. Minimum number of bright pixels required to update
#'   the object position. Default is `3`.
#' @param max_jump_px Numeric. Maximum allowed jump between consecutive frames,
#'   in pixels. Larger jumps are rejected. Default is `40`.
#' @param verbose Logical. If `TRUE`, prints progress messages. Default is
#'   `TRUE`.
#'
#' @return A data frame containing tracked object positions with columns
#'   `track_id`, `frame`, `x`, `y`, `n_pixels`, and `jump_px`.
#'
#' @examples
#' \dontrun{
#' tracks_bright <- track_selected_points_bright(
#'   frames,
#'   selected_objects,
#'   search_radius = 35,
#'   threshold_quantile = 0.97
#' )
#' }
#'
#' @importFrom dplyr bind_rows
#' @importFrom stats quantile
#' @export
track_selected_points_bright <- function(frames,
                                         selected_objects,
                                         frame_mats = NULL,
                                         search_radius = 35,
                                         threshold_quantile = 0.97,
                                         min_pixels = 3,
                                         max_jump_px = 40,
                                         verbose = TRUE) {

  if (nrow(selected_objects) == 0) {
    stop("No selected objects found.")
  }

  if (is.null(frame_mats)) {
    frame_mats <- lapply(frames, get_gray_matrix)
  }

  n_frames <- length(frames)
  all_tracks <- list()

  for (i in seq_len(nrow(selected_objects))) {

    if (verbose) {
      message("Tracking object ", i, " of ", nrow(selected_objects))
    }

    x_current <- selected_objects$x[i]
    y_current <- selected_objects$y[i]
    start_frame <- selected_objects$frame[i]

    this_track <- data.frame(
      track_id = selected_objects$selected_id[i],
      frame = start_frame,
      x = x_current,
      y = y_current,
      n_pixels = NA_integer_,
      jump_px = NA_real_
    )

    for (f in (start_frame + 1):n_frames) {

      mat <- frame_mats[[f]]

      x_min <- max(1, round(x_current - search_radius))
      x_max <- min(nrow(mat), round(x_current + search_radius))
      y_min <- max(1, round(y_current - search_radius))
      y_max <- min(ncol(mat), round(y_current + search_radius))

      local <- mat[x_min:x_max, y_min:y_max, drop = FALSE]

      # detect bright pixels in local search region
      thr <- stats::quantile(local, threshold_quantile, na.rm = TRUE)
      bw <- local >= thr

      idx <- which(bw, arr.ind = TRUE)

      if (nrow(idx) >= min_pixels) {

        # convert local coordinates back to full-frame coordinates
        cand_x <- x_min + idx[, 1] - 1
        cand_y <- y_min + idx[, 2] - 1

        # use centroid of bright pixels
        x_new <- mean(cand_x)
        y_new <- mean(cand_y)

        jump_px <- sqrt((x_new - x_current)^2 + (y_new - y_current)^2)

        # reject unrealistic jumps
        if (jump_px > max_jump_px) {
          x_new <- x_current
          y_new <- y_current
          jump_px <- 0
        }

      } else {

        # if no larva-like pixels found, keep previous position
        x_new <- x_current
        y_new <- y_current
        jump_px <- 0
      }

      this_track <- rbind(
        this_track,
        data.frame(
          track_id = selected_objects$selected_id[i],
          frame = f,
          x = x_new,
          y = y_new,
          n_pixels = ifelse(exists("idx"), nrow(idx), 0),
          jump_px = jump_px
        )
      )

      x_current <- x_new
      y_current <- y_new
    }

    all_tracks[[i]] <- this_track
  }

  dplyr::bind_rows(all_tracks)
}
