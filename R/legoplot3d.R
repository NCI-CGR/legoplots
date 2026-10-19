#' Plot a mutational-context "lego plot"
#'
#' Draw an SBS96 spectrum as a static projected 3D bar plot.
#'
#' @param contextdata Numeric mutation-context values; a SigProfiler matrix or
#'   data frame; or a path to a tab-delimited SigProfiler SBS96 matrix.
#' @param colors Palette: \code{"broad"}, \code{"sanger"}, \code{"mike"},
#'   or a custom vector of 6 colors (see \code{\link{lego_palette}}).
#' @param sample One or more sample or signature column names or numbers when
#'   `contextdata` is a SigProfiler matrix. Multiple selections are plotted
#'   side by side. May be omitted for a one-column matrix.
#' @param gap Fraction (0-1) of each grid cell left empty as a gap between bars.
#' @param zscale Height, in grid units, that the tallest bar should reach.
#' @param zmax Data value mapped to \code{zscale}; defaults to the maximum value
#'   across selected samples unless `free_z = TRUE`.
#' @param labels Draw "preceding base"/"succeeding base" axis labels.
#' @param zlabels Draw a numeric scale on the vertical axis.
#' @param zsub Descriptive label for the vertical axis (or \code{FALSE} for none).
#' @param gridlines Draw a floor grid under the bars.
#' @param main Optional plot title. For multiple samples, either one title to
#'   reuse or one title per sample. Sample names are used when this is `NULL`.
#' @param main_line Optional margin line for panel titles. Lower values move
#'   titles toward the plotting region; negative values move them inside it.
#'   `NULL` uses the base-R default.
#' @param mar Plot margins in lines, as `c(bottom, left, top, right)`. Defaults
#'   to `c(1, 1, 3, 1)` for titled plots and `c(1, 1, 1, 1)` otherwise.
#' @param legend Draw a boxed base-substitution color legend.
#' @param alpha Bar opacity between 0 (fully transparent) and 1 (opaque).
#' @param elevation Camera elevation in degrees, greater than 0 and less than 90.
#' @param azimuth Camera rotation around the plot in degrees, from 10 to 80.
#' @param outfile Output path, or \code{NA} for the current graphics device.
#' @param width,height Output size per panel in pixels (PNG) or inches
#'   (PDF/SVG, via \code{width/res}). Multi-sample output is `width` times the
#'   number of panel columns and `height` times the number of panel rows.
#' @param res Resolution in dpi, used for PNG output.
#' @param nrow,ncol Optional numbers of panel rows and columns for multiple
#'   samples. If only one is supplied, the other is calculated automatically.
#'   By default, all samples are placed in one row.
#' @param free_z Use a separate vertical scale for each panel. The default,
#'   `FALSE`, uses a shared scale so bar heights can be compared across panels.
#'   Cannot be combined with an explicit `zmax`.
#' @return For one sample, the computed layout data frame. For multiple
#'   samples, a named list of layout data frames. Returned invisibly.
#' @examples
#' x <- setNames(seq_len(96), context96$context)
#' legoplot3d(x, labels = TRUE, zsub = "Count")
#' \dontrun{
#' # Plot a sample directly from a SigProfiler SBS96 matrix.
#' legoplot3d("project.SBS96.all", sample = "Sample1", outfile = "sample1.pdf")
#' # Plot selected samples side by side.
#' legoplot3d("project.SBS96.all", sample = c("Sample1", "Sample2"))
#' # Stack the same samples vertically.
#' legoplot3d("project.SBS96.all", sample = c("Sample1", "Sample2"), ncol = 1)
#' # Give each sample its own vertical scale.
#' legoplot3d("project.SBS96.all", sample = c("Sample1", "Sample2"), free_z = TRUE)
#' }
#' @export
legoplot3d <- function(contextdata, colors = "sanger", sample = NULL, gap = 0.2,
                        zscale = 3, zmax = NULL,
                        labels = FALSE, zlabels = TRUE, zsub = FALSE, gridlines = TRUE,
                        main = NULL, legend = FALSE, alpha = 1,
                        elevation = 35.26439, azimuth = 45,
                        outfile = NA,
                        width = 1000, height = 800, res = 150,
                        nrow = NULL, ncol = NULL, free_z = FALSE,
                        mar = NULL, main_line = NULL) {

  samples <- resolve_legoplot_inputs(contextdata, sample = sample)
  layouts <- lapply(samples, compute_lego_layout, colors = colors)

  if (!is.logical(free_z) || length(free_z) != 1L || is.na(free_z)) {
    stop("free_z must be TRUE or FALSE.", call. = FALSE)
  }
  if (free_z && !is.null(zmax)) {
    stop("zmax cannot be supplied when free_z = TRUE.", call. = FALSE)
  }

  # Preserve the original single-sample behavior and return type.
  if (length(layouts) == 1L) {
    return(render_lego3d(
      layouts[[1L]], gap = gap, zscale = zscale, zmax = zmax,
      labels = labels, zlabels = zlabels, zsub = zsub, gridlines = gridlines,
      main = main, main_line = main_line, mar = mar,
      legend = legend, alpha = alpha,
      elevation = elevation, azimuth = azimuth,
      outfile = outfile, width = width, height = height, res = res
    ))
  }

  sample_names <- names(layouts)
  if (is.null(main)) {
    panel_titles <- sample_names
  } else if (is.character(main) && !anyNA(main) &&
             length(main) %in% c(1L, length(layouts))) {
    panel_titles <- rep(main, length.out = length(layouts))
  } else {
    stop("main must be NULL, one title, or one title per selected sample.",
         call. = FALSE)
  }

  # Use a common vertical scale so side-by-side heights are comparable.
  panel_zmax <- zmax
  if (!free_z && is.null(panel_zmax)) {
    maximum <- max(vapply(layouts, function(x) max(x$value), numeric(1)))
    if (maximum > 0) panel_zmax <- maximum
  }

  panel_grid <- resolve_panel_grid(length(layouts), nrow = nrow, ncol = ncol)

  use_current_device <- length(outfile) == 1L && is.na(outfile)
  if (use_current_device) {
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
  } else {
    open_legoplot_device(
      outfile,
      width = width * panel_grid[["ncol"]],
      height = height * panel_grid[["nrow"]],
      res = res
    )
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  graphics::par(mfrow = unname(panel_grid))
  for (i in seq_along(layouts)) {
    render_lego3d(
      layouts[[i]], gap = gap, zscale = zscale, zmax = panel_zmax,
      labels = labels, zlabels = zlabels, zsub = zsub, gridlines = gridlines,
      main = panel_titles[[i]], main_line = main_line, mar = mar,
      legend = legend, alpha = alpha,
      elevation = elevation, azimuth = azimuth, outfile = NA,
      width = width, height = height, res = res
    )
  }
  invisible(layouts)
}

resolve_panel_grid <- function(nplots, nrow = NULL, ncol = NULL) {
  valid_dimension <- function(x) {
    is.null(x) || (is.numeric(x) && length(x) == 1L && is.finite(x) &&
      x >= 1 && x == as.integer(x))
  }
  if (!valid_dimension(nrow) || !valid_dimension(ncol)) {
    stop("nrow and ncol must be NULL or positive whole numbers.", call. = FALSE)
  }

  if (is.null(nrow) && is.null(ncol)) {
    nrow <- 1L
    ncol <- nplots
  } else if (is.null(nrow)) {
    ncol <- as.integer(ncol)
    nrow <- ceiling(nplots / ncol)
  } else if (is.null(ncol)) {
    nrow <- as.integer(nrow)
    ncol <- ceiling(nplots / nrow)
  } else {
    nrow <- as.integer(nrow)
    ncol <- as.integer(ncol)
  }

  if (nrow * ncol < nplots) {
    stop("nrow * ncol must provide enough panels for all selected samples.",
         call. = FALSE)
  }
  c(nrow = nrow, ncol = ncol)
}

resolve_legoplot_inputs <- function(contextdata, sample = NULL) {
  if (is.numeric(contextdata) && is.null(dim(contextdata))) {
    if (!is.null(sample)) {
      stop("sample is only used with SigProfiler files, matrices, or data frames.",
           call. = FALSE)
    }
    return(list(contextdata))
  }

  selected <- select_sigprofiler_samples(
    read_sigprofiler_sbs96(contextdata), sample = sample
  )
  samples <- lapply(seq_len(ncol(selected)), function(i) {
    stats::setNames(as.numeric(selected[, i]), rownames(selected))
  })
  names(samples) <- colnames(selected)
  samples
}

open_legoplot_device <- function(outfile, width, height, res) {
  dimensions <- c(width = width, height = height, res = res)
  if (!is.numeric(dimensions) || any(!is.finite(dimensions)) ||
      any(dimensions <= 0)) {
    stop("width, height, and res must be positive numbers.", call. = FALSE)
  }
  if (!is.character(outfile) || length(outfile) != 1L || !nzchar(outfile)) {
    stop("outfile must be NA or one non-empty file path.", call. = FALSE)
  }

  ext <- tolower(tools::file_ext(outfile))
  if (!ext %in% c("png", "pdf", "svg")) {
    stop("outfile must use a .png, .pdf, or .svg extension.", call. = FALSE)
  }
  previous_device <- grDevices::dev.cur()
  switch(ext,
    pdf = grDevices::pdf(outfile, width = width / res, height = height / res),
    svg = grDevices::svg(outfile, width = width / res, height = height / res),
    grDevices::png(outfile, width = width, height = height, res = res)
  )
  if (identical(grDevices::dev.cur(), previous_device)) {
    stop("The ", toupper(ext), " graphics device could not be opened.",
         call. = FALSE)
  }
  invisible()
}

resolve_legoplot_input <- function(contextdata, sample = NULL) {
  if (!(is.numeric(contextdata) && is.null(dim(contextdata)))) {
    return(select_sigprofiler_sample(
      read_sigprofiler_sbs96(contextdata),
      sample = sample
    ))
  }
  if (!is.null(sample)) {
    stop("sample is only used with SigProfiler files, matrices, or data frames.",
         call. = FALSE)
  }
  contextdata
}
