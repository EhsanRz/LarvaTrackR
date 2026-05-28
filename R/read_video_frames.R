#' Read video frames into R
#'
#' Extracts frames from a video file at a defined frame rate and loads them as
#' EBImage image objects. The extracted frame files are saved in an output
#' directory, and the loaded frames are returned as a list.
#'
#' @param video_path Character. Path to the input video file.
#' @param output_dir Character. Directory where extracted frame images will be
#'   saved. Default is a temporary directory.
#' @param fps Numeric. Frames per second to extract from the video. Default is
#'   `5`.
#' @param format Character. Image format for extracted frames. Default is
#'   `"png"`.
#' @param start_time Numeric. Start time in seconds for frame extraction.
#'   Default is `0`.
#' @param duration Numeric or `NULL`. Duration in seconds to extract from the
#'   video. If `NULL`, frames are extracted from the full video.
#'
#' @return A list of EBImage image objects. The list also contains attributes:
#'   `frame_files`, `output_dir`, `fps`, `start_time`, and `duration`.
#'
#' @examples
#' \dontrun{
#' frames <- read_video_frames(
#'   video_path = "example_video.mp4",
#'   fps = 5
#' )
#' }
#'
#' @importFrom av av_video_images
#' @importFrom EBImage readImage
#' @importFrom utils packageVersion
#' @export
read_video_frames <- function(video_path,
output_dir = tempfile("video_frames_"),
fps = 5,
format = "png",
start_time = 0,
duration = NULL) {

  if (!file.exists(path.expand(video_path))) {
    stop("Video file not found: ", video_path)
  }

  video_path <- path.expand(video_path)

  if (!requireNamespace("av", quietly = TRUE)) {
    stop("Package 'av' is required. Install it with install.packages('av').")
  }

  if (!requireNamespace("EBImage", quietly = TRUE)) {
    stop("Package 'EBImage' is required. Install it with BiocManager::install('EBImage').")
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  if (is.null(duration)) {
    av::av_video_images(
      video = video_path,
      destdir = output_dir,
      format = format,
      fps = fps
    )
  } else {
    av::av_video_images(
      video = video_path,
      destdir = output_dir,
      format = format,
      fps = fps,
      start_time = start_time,
      duration = duration
    )
  }

  files <- list.files(
    output_dir,
    pattern = paste0("\\.", format, "$"),
    full.names = TRUE
  )

  files <- sort(files)

  if (length(files) == 0) {
    stop("No frames were extracted from the video.")
  }

  frames <- lapply(files, EBImage::readImage)
  names(frames) <- basename(files)

  attr(frames, "frame_files") <- files
  attr(frames, "output_dir") <- output_dir
  attr(frames, "fps") <- fps
  attr(frames, "start_time") <- start_time
  attr(frames, "duration") <- duration

  message(length(frames), " frames extracted and loaded.")

  return(frames)
}
