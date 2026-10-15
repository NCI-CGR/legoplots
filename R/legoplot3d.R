#' Plot a mutational-context "lego plot"
#'
#' Draw an SBS96 spectrum as a static projected 3D bar plot.
#'
#' @param contextdata Numeric mutation-context values; a SigProfiler matrix or
#'   data frame; or a path to a tab-delimited SigProfiler SBS96 matrix.
#' @param sixcolors Palette: \code{"broad"}, \code{"sanger"}, \code{"mike"},
#'   or a custom vector of 6 colors (see \code{\link{lego_palette}}).
#' @param sample Sample or signature column name or number when `contextdata`
#'   is a SigProfiler matrix. May be omitted for a one-column matrix.
#' @param gap Fraction (0-1) of each grid cell left empty as a gap between bars.
#' @param zscale Height, in grid units, that the tallest bar should reach.
#' @param zmax Data value mapped to \code{zscale}; defaults to the maximum value.
#' @param labels Draw "preceding base"/"succeeding base" axis labels.
#' @param zlabels Draw a numeric scale on the vertical axis.
#' @param zsub Descriptive label for the vertical axis (or \code{FALSE} for none).
#' @param gridlines Draw a floor grid under the bars.
#' @param main Optional plot title.
#' @param legend Draw a boxed base-substitution color legend.
#' @param alpha Bar opacity between 0 (fully transparent) and 1 (opaque).
#' @param elevation Camera elevation in degrees, greater than 0 and less than 90.
#' @param azimuth Camera rotation around the plot in degrees, from 10 to 80.
#' @param outfile Output path, or \code{NA} for the current graphics device.
#' @param width,height Output size in pixels (PNG) or inches (PDF/SVG, via \code{width/res}).
#' @param res Resolution in dpi, used for PNG output.
#' @return The computed layout data frame, invisibly.
#' @examples
#' x <- setNames(seq_len(96), context96$context)
#' legoplot3d(x, sixcolors = "mike", labels = TRUE, zsub = "Count")
#' \dontrun{
#' # Plot a sample directly from a SigProfiler SBS96 matrix.
#' legoplot3d("project.SBS96.all", sample = "Sample1", outfile = "sample1.pdf")
#' }
#' @export
legoplot3d <- function(contextdata, sixcolors = "mike", sample = NULL, gap = 0.2,
                        zscale = 3, zmax = NULL,
                        labels = FALSE, zlabels = TRUE, zsub = FALSE, gridlines = TRUE,
                        main = NULL, legend = FALSE, alpha = 1,
                        elevation = 35.26439, azimuth = 45,
                        outfile = NA,
                        width = 1000, height = 800, res = 150) {

  # Select a column when given a SigProfiler file or matrix.
  if (!(is.numeric(contextdata) && is.null(dim(contextdata)))) {
    contextdata <- select_sigprofiler_sample(
      read_sigprofiler_sbs96(contextdata),
      sample = sample
    )
  } else if (!is.null(sample)) {
    stop("sample is only used with SigProfiler files, matrices, or data frames.",
         call. = FALSE)
  }

  # Resolve values, colors, and grid positions.
  layout <- compute_lego_layout(contextdata, sixcolors = sixcolors)

  # Render the completed layout.
  render_lego3d(layout, gap = gap, zscale = zscale, zmax = zmax,
                labels = labels, zlabels = zlabels, zsub = zsub, gridlines = gridlines,
                main = main, legend = legend, alpha = alpha,
                elevation = elevation, azimuth = azimuth,
                outfile = outfile,
                width = width, height = height, res = res)
}
