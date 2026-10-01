#' Plot a mutational-context "lego plot"
#'
#' Draws the 96-trinucleotide-context mutational spectrum of a sample or
#' signature as a 3D-style bar plot in the style used by Mike Lawrence's lab
#' (MGH), as an alternative to the flat 2D spectrum plots used by
#' COSMIC/Sanger. Rendered as a static isometric image using only base R
#' graphics (see \code{lego_render.R}) -- no rgl, no OpenGL, no display
#' server required, and no dependency on the archived \pkg{barplot3d}
#' package.
#'
#' This is a thin wrapper around \code{\link{compute_lego_layout}} (decides
#' what to draw: bar heights, positions, colors) and
#' \code{\link{render_lego3d}} (draws it). Call those directly if you want
#' the layout data without any graphics, want to inspect/test it, or want to
#' render it with a different backend.
#'
#' @param contextdata A numeric vector of the 96 mutation-context values.
#'   Recommended: a named vector using standard SBS96 notation, e.g.
#'   \code{names(x) <- context96$context} (like \code{"A[C>A]A"}); order then
#'   doesn't matter and mismatched names are caught with an error instead of
#'   silently producing a wrong plot. An unnamed vector is also accepted but
#'   must have exactly 96 elements in the standard order used by
#'   \code{sigdata1.txt} (see \code{context96.R}).
#' @param sixcolors Palette: \code{"broad"}, \code{"sanger"}, \code{"mike"},
#'   or a custom vector of 6 hex colors (see \code{\link{lego_palette}}).
#' @param gap Fraction (0-1) of each grid cell left empty as a gap between bars.
#' @param zscale Height, in grid units, that the tallest bar should reach.
#' @param zmax The data value that maps to \code{zscale}; defaults to
#'   \code{max(contextdata)}. Set explicitly to keep the vertical scale
#'   comparable across multiple plots.
#' @param labels Draw "preceding base"/"succeeding base" axis labels.
#' @param zlabels Draw a numeric scale on the vertical axis.
#' @param zsub Descriptive label for the vertical axis (or \code{FALSE} for none).
#' @param gridlines Draw a floor grid under the bars.
#' @param main Optional plot title.
#' @param outfile If not \code{NA}, save the plot (PNG/PDF/SVG, by extension)
#'   to this path -- this works with no graphics display at all.
#' @param width,height Output size in pixels (PNG) or inches (PDF/SVG, via \code{width/res}).
#' @param res Resolution in dpi, used for PNG output.
#' @return The computed layout data frame, invisibly.
#' @examples
#' \dontrun{
#' sigdata <- read.table("sigdata1.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
#' x <- sigdata$Signature_2
#' names(x) <- context96$context   # sigdata1.txt is already in context96's standard order
#' legoplot3d(x, sixcolors = "mike", labels = TRUE, zsub = "Probability")
#'
#' # Headless / CLI / CI, no graphics display needed:
#' legoplot3d(x, outfile = "sig2.png")
#' }
#' @export
legoplot3d <- function(contextdata, sixcolors = "mike", gap = 0.2, zscale = 3, zmax = NULL,
                        labels = FALSE, zlabels = TRUE, zsub = FALSE, gridlines = TRUE,
                        main = NULL, outfile = NA, width = 1000, height = 800, res = 150) {

  layout <- compute_lego_layout(contextdata, sixcolors = sixcolors)

  render_lego3d(layout, gap = gap, zscale = zscale, zmax = zmax,
                labels = labels, zlabels = zlabels, zsub = zsub, gridlines = gridlines,
                main = main, outfile = outfile, width = width, height = height, res = res)
}
