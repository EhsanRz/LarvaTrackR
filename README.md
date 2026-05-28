LarvaTrackR
================

# LarvaTrackR <img src="man/figures/larvatrackr_logo.png" align="right" width="180" />

LarvaTrackR is an R package for larval movement tracking from video
files. It allows you to extract video frames, select regions of
interest, calibrate pixel size, track larvae or other moving objects,
calculate movement parameters, and generate movement plots.

# LarvaTrackR

LarvaTrackR is an R package for larval movement tracking from video
files.

It allows you to extract video frames, select regions of interest,
calibrate pixel size, track larvae or other moving objects, calculate
movement parameters, and generate movement plots.

## Installation

First install the required installer packages:

``` r
install.packages("remotes")
install.packages("BiocManager")
BiocManager::install("EBImage")
```

Then install LarvaTrackR from GitHub:

``` r
remotes::install_github("YOUR_GITHUB_USERNAME/LarvaTrackR")
```

Replace `YOUR_GITHUB_USERNAME` with the GitHub username where the
package is uploaded.

Then load the package:

``` r
library(LarvaTrackR)
```

## Basic example

``` r
library(LarvaTrackR)

frames <- read_video_frames(
  video_path = "path/to/your/video.mp4",
  fps = 5
)

roi <- select_roi(
  frames,
  frame_number = 1
)

frames_roi <- crop_frames_to_roi(
  frames,
  roi
)

pixel_size_mm <- calibrate_scale(
  frames_roi,
  frame_number = 1,
  known_distance_mm = 10
)

selected_objects <- select_objects(
  frames_roi,
  frame_number = 1
)

tracks <- track_selected_points(
  frames_roi,
  selected_objects,
  template_radius = 12,
  search_radius = 30
)

tracks_movement <- calculate_movement_mm_clean(
  tracks,
  fps = 5,
  pixel_size_mm = pixel_size_mm,
  min_step_px = 1,
  max_step_px = 10,
  remove_first_frames = 10
)

plot_total_distance(tracks_movement)

plot_speed_over_time(
  tracks_movement,
  individual = TRUE,
  max_speed = 20
)
```

## Main functions

| Function                        | Purpose                              |
|---------------------------------|--------------------------------------|
| `read_video_frames()`           | Extract video frames                 |
| `select_roi()`                  | Select a region of interest          |
| `crop_frames_to_roi()`          | Crop frames to the selected ROI      |
| `calibrate_scale()`             | Calculate mm per pixel               |
| `select_objects()`              | Select larvae or objects to track    |
| `track_selected_points()`       | Track selected objects across frames |
| `calculate_movement_mm_clean()` | Calculate speed and distance         |
| `plot_total_distance()`         | Plot cumulative movement             |
| `plot_speed_over_time()`        | Plot speed over time                 |
| `add_freezing_status()`         | Detect freezing-like behaviour       |

## Note

Interactive functions such as `select_roi()`, `calibrate_scale()`, and
`select_objects()` require clicking on the plot. Run these steps
interactively in RStudio rather than during automatic knitting.
