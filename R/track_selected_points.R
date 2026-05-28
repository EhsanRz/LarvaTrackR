#' Track selected points across video frames
#'
#' Tracks manually selected objects across a sequence of video frames using
#' template matching. For each selected object, a small image patch is extracted
#' from the starting frame and searched in subsequent frames within a defined
#' search radius.
#'
#' @param frames A list of image frames, usually loaded from a video.
#' @param selected_objects A data frame of manually selected objects. It should
#'   contain `selected_id`, `frame`, `x`, and `y` columns.
#' @param frame_mats Optional list of precomputed grayscale image matrices. If
#'   `NULL`, frames are converted internally using `get_gray_matrix()`.
#' @param template_radius Numeric. Radius of the template patch around each
#'   selected object, in pixels. Default is `12`.
#' @param search_radius Numeric. Radius around the previous position to search
#'   for the object in the next frame, in pixels. Default is `30`.
#' @param update_template Logical. If `TRUE`, the template is updated after each
#'   tracked frame. Default is `TRUE`.
#' @param flip_y Logical. If `TRUE`, flips the selected y coordinates before
#'   tracking. Default is `FALSE`.
#' @param verbose Logical. If `TRUE`, prints progress messages. Default is
#'   `TRUE`.
#'
#' @return A data frame containing tracked object positions with columns
#'   `track_id`, `frame`, `x`, `y`, and `score`.
#'
#' @examples
#' \dontrun{
#' tracks <- track_selected_points(
#'   frames,
#'   selected_objects,
#'   template_radius = 12,
#'   search_radius = 30
#' )
#' }
#'
#' @importFrom dplyr bind_rows
#' @export
track_selected_points <- function(frames,
                                  selected_objects,
                                  frame_mats = NULL,
                                  template_radius = 12,
                                  search_radius = 30,
                                  update_template = TRUE,
                                  flip_y = FALSE,
                                  verbose = TRUE) {

  if (nrow(selected_objects) == 0) {
    stop("No selected objects found.")
  }

  n_frames <- length(frames)

  if (is.null(frame_mats)) {
    if (verbose) message("Converting frames to grayscale matrices...")
    frame_mats <- lapply(frames, get_gray_matrix)
  } else {
    if (verbose) message("Using existing frame_mats.")
  }

  selected_use <- selected_objects

  if (flip_y) {
    first_mat <- frame_mats[[selected_use$frame[1]]]
    h <- ncol(first_mat)
    selected_use$y <- h - selected_use$y + 1
  }

  if (verbose) {
    message("Number of frames: ", n_frames)
    message("Number of selected objects: ", nrow(selected_use))
    message("Template radius: ", template_radius)
    message("Search radius: ", search_radius)
    message("Starting tracking...")
  }

  all_tracks <- list()

  for (i in seq_len(nrow(selected_use))) {

    if (verbose) {
      message("Tracking object ", i, " of ", nrow(selected_use))
    }

    x_current <- selected_use$x[i]
    y_current <- selected_use$y[i]
    start_frame <- selected_use$frame[i]

    template <- crop_patch(
      frame_mats[[start_frame]],
      x_current,
      y_current,
      radius = template_radius
    )

    this_track <- data.frame(
      track_id = selected_use$selected_id[i],
      frame = start_frame,
      x = x_current,
      y = y_current,
      score = NA_real_
    )

    if (start_frame < n_frames) {

      for (f in (start_frame + 1):n_frames) {

        if (verbose && f %% 5 == 0) {
          message("  Object ", i, ": frame ", f, " / ", n_frames)
        }

        mat <- frame_mats[[f]]

        best_score <- Inf
        best_x <- x_current
        best_y <- y_current

        x_range <- seq(
          max(1 + template_radius, round(x_current - search_radius)),
          min(nrow(mat) - template_radius, round(x_current + search_radius))
        )

        y_range <- seq(
          max(1 + template_radius, round(y_current - search_radius)),
          min(ncol(mat) - template_radius, round(y_current + search_radius))
        )

        for (xx in x_range) {
          for (yy in y_range) {

            candidate <- crop_patch(mat, xx, yy, radius = template_radius)
            score <- patch_score(template, candidate)

            if (score < best_score) {
              best_score <- score
              best_x <- xx
              best_y <- yy
            }
          }
        }

        x_current <- best_x
        y_current <- best_y

        this_track <- rbind(
          this_track,
          data.frame(
            track_id = selected_use$selected_id[i],
            frame = f,
            x = x_current,
            y = y_current,
            score = best_score
          )
        )

        if (update_template) {
          template <- crop_patch(
            mat,
            x_current,
            y_current,
            radius = template_radius
          )
        }
      }
    }

    all_tracks[[i]] <- this_track

    if (verbose) {
      message("Finished object ", i)
    }
  }

  tracks <- dplyr::bind_rows(all_tracks)

  if (verbose) {
    message("Tracking finished.")
    message("Total tracked points: ", nrow(tracks))
    message("Total tracks: ", length(unique(tracks$track_id)))
  }

  return(tracks)
}
