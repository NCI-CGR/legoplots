#' Animate a mutational-context lego plot
#'
#' Render a sequence of lego plots along a closed elliptical camera path and
#' encode the PNG frames as an animated GIF. Frame rendering uses base R;
#' GIF encoding uses the optional `gifski` package.
#'
#' @param contextdata Numeric mutation-context values, a SigProfiler matrix or
#'   data frame, or a path to a tab-delimited SigProfiler SBS96 matrix.
#' @param outfile Path to the output GIF file.
#' @param colors Palette passed to [lego_palette()].
#' @param sample Sample or signature column name or number when `contextdata`
#'   is a SigProfiler matrix.
#' @param n_frames Number of animation frames. Must be at least 2.
#' @param fps Frames per second.
#' @param azimuth_center,elevation_center Center of the camera path in degrees.
#' @param azimuth_amplitude,elevation_amplitude Radius of the camera path on
#'   each axis in degrees. The resulting angles must remain within the ranges
#'   accepted by [render_lego3d()].
#' @param width,height Frame dimensions in pixels.
#' @param res PNG resolution in dots per inch.
#' @param loop Whether the GIF should loop continuously.
#' @param progress Whether `gifski` should display encoding progress.
#' @param ... Additional arguments passed to [render_lego3d()], such as
#'   `labels`, `zsub`, `main`, `legend`, `alpha`, `gap`, and `zmax`. Camera
#'   angles and output-device arguments are controlled by this function.
#' @return The normalized path to `outfile`, invisibly.
#' @examples
#' \dontrun{
#' x <- setNames(seq_len(96), context96$context)
#' animate_legoplot3d(
#'   x,
#'   outfile = "legoplot.gif",
#'   labels = TRUE,
#'   zsub = "Count",
#'   main = "Example SBS96 profile"
#' )
#' }
#' @export
animate_legoplot3d <- function(contextdata, outfile = "legoplot.gif",
                                colors = "sanger", sample = NULL,
                                n_frames = 120, fps = 30,
                                azimuth_center = 45,
                                elevation_center = 35,
                                azimuth_amplitude = 8,
                                elevation_amplitude = 8,
                                width = 800, height = 640, res = 120,
                                loop = TRUE, progress = interactive(), ...) {
  validate_animation_options(
    outfile = outfile,
    n_frames = n_frames,
    fps = fps,
    azimuth_center = azimuth_center,
    elevation_center = elevation_center,
    azimuth_amplitude = azimuth_amplitude,
    elevation_amplitude = elevation_amplitude,
    width = width,
    height = height,
    res = res,
    loop = loop,
    progress = progress
  )
  if (!requireNamespace("gifski", quietly = TRUE)) {
    stop(
      "animate_legoplot3d() requires the optional 'gifski' package. ",
      "Install it with install.packages(\"gifski\").",
      call. = FALSE
    )
  }
  outfile <- path.expand(outfile)
  if (!dir.exists(dirname(outfile))) {
    stop("The output directory does not exist.", call. = FALSE)
  }

  contextdata <- resolve_legoplot_input(contextdata, sample = sample)
  layout <- compute_lego_layout(contextdata, colors = colors)
  camera <- orbit_camera_path(
    n_frames = n_frames,
    azimuth_center = azimuth_center,
    elevation_center = elevation_center,
    azimuth_amplitude = azimuth_amplitude,
    elevation_amplitude = elevation_amplitude
  )
  frame_dir <- tempfile("legoplot_frames_")
  if (!dir.create(frame_dir)) {
    stop("Could not create a temporary frame directory.", call. = FALSE)
  }
  on.exit(unlink(frame_dir, recursive = TRUE, force = TRUE), add = TRUE)
  digits <- max(3L, nchar(as.character(n_frames)))
  frame_files <- file.path(
    frame_dir,
    sprintf(paste0("frame_%0", digits, "d.png"), seq_len(n_frames))
  )

  for (i in seq_len(n_frames)) {
    render_lego3d(
      layout,
      elevation = camera$elevation[i],
      azimuth = camera$azimuth[i],
      outfile = frame_files[i],
      width = width,
      height = height,
      res = res,
      ...
    )
  }

  gifski::gifski(
    png_files = frame_files,
    gif_file = outfile,
    width = as.integer(width),
    height = as.integer(height),
    delay = 1 / fps,
    loop = loop,
    progress = progress
  )
  if (!file.exists(outfile) || file.info(outfile)$size <= 0) {
    stop("GIF encoding did not produce a non-empty output file.", call. = FALSE)
  }
  invisible(normalizePath(outfile, mustWork = TRUE))
}

orbit_camera_path <- function(n_frames, azimuth_center, elevation_center,
                              azimuth_amplitude, elevation_amplitude) {
  angle <- 2 * pi * (seq_len(n_frames) - 1L) / n_frames
  data.frame(
    azimuth = azimuth_center + azimuth_amplitude * sin(angle),
    elevation = elevation_center + elevation_amplitude * cos(angle)
  )
}

validate_animation_options <- function(outfile, n_frames, fps,
                                       azimuth_center, elevation_center,
                                       azimuth_amplitude, elevation_amplitude,
                                       width, height, res, loop, progress) {
  if (!is.character(outfile) || length(outfile) != 1L || is.na(outfile) ||
      !nzchar(outfile) || tolower(tools::file_ext(outfile)) != "gif") {
    stop("outfile must be one non-empty path with a .gif extension.", call. = FALSE)
  }
  if (!is.numeric(n_frames) || length(n_frames) != 1L || !is.finite(n_frames) ||
      n_frames < 2L || n_frames > .Machine$integer.max ||
      n_frames != floor(n_frames)) {
    stop("n_frames must be an integer of at least 2.", call. = FALSE)
  }
  if (!is.numeric(fps) || length(fps) != 1L || !is.finite(fps) || fps <= 0) {
    stop("fps must be a single positive number.", call. = FALSE)
  }
  camera <- c(
    azimuth_center = azimuth_center,
    elevation_center = elevation_center,
    azimuth_amplitude = azimuth_amplitude,
    elevation_amplitude = elevation_amplitude
  )
  if (!is.numeric(camera) || any(!is.finite(camera)) ||
      azimuth_amplitude < 0 || elevation_amplitude < 0) {
    stop("Camera centers and amplitudes must be finite; amplitudes cannot be negative.",
         call. = FALSE)
  }
  if (azimuth_center - azimuth_amplitude < 10 ||
      azimuth_center + azimuth_amplitude > 80) {
    stop("The animated azimuth must remain between 10 and 80 degrees.",
         call. = FALSE)
  }
  if (elevation_center - elevation_amplitude <= 0 ||
      elevation_center + elevation_amplitude >= 90) {
    stop("The animated elevation must remain above 0 and below 90 degrees.",
         call. = FALSE)
  }
  dimensions <- c(width = width, height = height)
  if (!is.numeric(dimensions) || any(!is.finite(dimensions)) ||
      any(dimensions <= 0) || any(dimensions > .Machine$integer.max) ||
      any(dimensions != floor(dimensions))) {
    stop("width and height must be positive integers.", call. = FALSE)
  }
  if (!is.numeric(res) || length(res) != 1L || !is.finite(res) || res <= 0) {
    stop("res must be a single positive number.", call. = FALSE)
  }
  logical_options <- list(loop = loop, progress = progress)
  if (!all(vapply(logical_options, function(x) {
    is.logical(x) && length(x) == 1L && !is.na(x)
  }, logical(1)))) {
    stop("loop and progress must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(TRUE)
}
