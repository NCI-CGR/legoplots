## Base-graphics ("2.5D" isometric) renderer for lego plots.
##
## This deliberately avoids rgl entirely: it projects each 3D bar onto a 2D
## plane by hand and draws it with graphics::polygon(), so the whole plot is
## just base R (grDevices + graphics, both always present). That means it
## needs no OpenGL, no X11, no headless-Chrome workaround, and no
## architecture-specific compiled package at all -- a plain png()/pdf()
## device is sufficient anywhere R runs (a laptop, a bare Docker image, a
## cluster node with no display), on x86_64 or arm64 alike.
##
## The trade-off vs. rgl: this produces a static image, not an interactive,
## rotatable scene.

#' Darken/lighten a hex color by a brightness factor
#'
#' @param hex A color, e.g. "#805D3F".
#' @param factor Multiplier applied to each RGB channel (values below 1 darken).
#' @return A hex color string.
#' @keywords internal
shade_color <- function(hex, factor) {
  m <- grDevices::col2rgb(hex) * factor
  m[m > 255] <- 255
  m[m < 0] <- 0
  m <- round(m)
  grDevices::rgb(m[1, ], m[2, ], m[3, ], maxColorValue = 255)
}

#' Project a 3D point onto the 2D isometric drawing plane
#'
#' Standard axonometric ("isometric tile") projection: \code{x} and \code{y}
#' are the two horizontal grid axes (columns/rows of the lego grid), \code{z}
#' is bar height. The camera looks down at the grid from above one corner, so
#' the cell with the largest \code{x + y} is nearest the viewer (bottom of
#' the image) and \code{(0, 0)} is farthest (top of the image); increasing
#' \code{z} moves a point straight up the image.
#'
#' @param x,y,z Numeric vectors of grid/height coordinates.
#' @return A list with elements \code{sx}, \code{sy} (screen coordinates).
#' @keywords internal
iso_project <- function(x, y, z = 0) {
  cos30 <- sqrt(3) / 2
  sin30 <- 0.5
  ## sx is negated (y - x, not x - y) purely to mirror left/right so screen
  ## position matches the reference renders (sig9.png, lego_ref.png); sy is
  ## untouched so front/back depth ordering (which was already correct) is
  ## unaffected.
  list(sx = (y - x) * cos30, sy = -(x + y) * sin30 + z)
}

#' Draw one 3D bar as three shaded isometric faces
#'
#' Only the top face and the two side faces touching the bar's nearest
#' vertical edge (max-x and max-y) are visible from the camera used by
#' \code{\link{iso_project}}, so only those three quads are drawn.
#'
#' @param x0,x1,y0,y1 Footprint of the bar in grid units.
#' @param z1 Bar height in grid units (base is always at \code{z = 0}).
#' @param topcolor Base color of the bar; the two side faces are automatically
#'   shaded darker/lighter versions of it.
#' @param border Outline color for each face, or \code{NA} for none.
#' @param shade_top,shade_front,shade_right Brightness multipliers (see \code{\link{shade_color}}).
#' @keywords internal
draw_iso_bar <- function(x0, x1, y0, y1, z1, topcolor, border = "grey15",
                          shade_top = 1, shade_front = 0.8, shade_right = 0.6) {
  quad <- function(x, y, z, col) {
    p <- iso_project(x, y, z)
    graphics::polygon(p$sx, p$sy, col = col, border = border, lwd = 0.6)
  }
  if (z1 <= 0) return(invisible())

  ## Right face (x = x1)
  quad(c(x1, x1, x1, x1), c(y0, y1, y1, y0), c(0, 0, z1, z1),
       shade_color(topcolor, shade_right))
  ## Front face (y = y1)
  quad(c(x0, x1, x1, x0), c(y1, y1, y1, y1), c(0, 0, z1, z1),
       shade_color(topcolor, shade_front))
  ## Top face (z = z1)
  quad(c(x0, x1, x1, x0), c(y0, y0, y1, y1), c(z1, z1, z1, z1),
       shade_color(topcolor, shade_top))
  invisible()
}

#' Render a lego layout with base graphics (no rgl, no display required)
#'
#' Draws the 3D bars described by a layout data frame (see
#' \code{\link{compute_lego_layout}}) as a static isometric ("2.5D") image,
#' using nothing but \pkg{graphics}/\pkg{grDevices}. Works identically in an
#' interactive session, a plain \code{Rscript} call, or a headless
#' container/CI job -- the only requirement is a graphics device, and
#' \code{png()}/\code{pdf()} (used automatically when \code{outfile} is set)
#' are always available in base R.
#'
#' @param layout A data frame as returned by \code{\link{compute_lego_layout}}.
#' @param gap Fraction (0-1) of each grid cell left empty as a gap between bars.
#' @param zscale Height, in grid units, that the tallest bar should reach.
#'   Grid cells are 1 unit square, so the default of 3 gives bars a similar
#'   footprint-to-height ratio to a real lego brick.
#' @param zmax The data value that should map to \code{zscale}. Defaults to
#'   \code{max(layout$value)}; set this explicitly (e.g. to the same number
#'   across several calls) to keep the vertical scale comparable across
#'   multiple plots -- important when comparing signatures side by side.
#' @param labels Draw "preceding base" / "succeeding base" axis labels.
#' @param zlabels Draw a numeric scale on the vertical (z) axis.
#' @param zsub Descriptive label for the vertical axis, or \code{FALSE} for none.
#' @param gridlines Draw a floor grid under the bars.
#' @param main Optional plot title.
#' @param shade_top,shade_front,shade_right Brightness multipliers for the
#'   three visible faces of each bar (see \code{\link{shade_color}}).
#' @param border Outline color for bar faces.
#' @param outfile If not \code{NA}, save the plot to this path instead of the
#'   current graphics device. The format is taken from the file extension
#'   (".png", ".pdf", ".svg", ...); PNG is used if there is none.
#' @param width,height Output size in pixels (PNG) or inches (PDF/SVG).
#' @param res Resolution in dpi, used for PNG output.
#' @return The \code{layout} data frame, invisibly.
#' @examples
#' layout <- compute_lego_layout(setNames(runif(96), context96$context), sixcolors = "mike")
#' render_lego3d(layout, labels = TRUE, zsub = "Probability")
#' \dontrun{
#' render_lego3d(layout, outfile = "sig.png")   # works with no display at all
#' }
#' @export
render_lego3d <- function(layout, gap = 0.2, zscale = 3, zmax = NULL,
                           labels = FALSE, zlabels = TRUE, zsub = FALSE,
                           gridlines = TRUE, main = NULL,
                           shade_top = 1, shade_front = 0.8, shade_right = 0.6,
                           border = "grey15",
                           outfile = NA, width = 1000, height = 800, res = 150) {

  rows <- max(layout$row)
  cols <- max(layout$col)

  if (is.null(zmax)) zmax <- max(layout$value, na.rm = TRUE)
  heightscale <- if (isTRUE(zmax > 0)) zscale / zmax else 0

  if (!is.na(outfile)) {
    ext <- tolower(tools::file_ext(outfile))
    dev_open <- switch(ext,
      pdf = function() grDevices::pdf(outfile, width = width / res, height = height / res),
      svg = function() grDevices::svg(outfile, width = width / res, height = height / res),
      function() grDevices::png(outfile, width = width, height = height, res = res)
    )
    dev_open()
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  corners <- expand.grid(x = c(0, cols), y = c(0, rows), z = c(0, zscale))
  p <- iso_project(corners$x, corners$y, corners$z)
  pad <- 0.08 * max(diff(range(p$sx)), diff(range(p$sy)))
  graphics::par(mar = c(1, 1, if (is.null(main)) 1 else 3, 1))
  graphics::plot(NA, xlim = range(p$sx) + c(-pad, pad) * 2.2,
                 ylim = range(p$sy) + c(-pad, pad * 1.3),
                 asp = 1, axes = FALSE, xlab = "", ylab = "")
  if (!is.null(main)) graphics::title(main = main)

  if (gridlines) {
    for (r in 0:rows) {
      pr <- iso_project(c(0, cols), c(r, r), 0)
      graphics::lines(pr$sx, pr$sy, col = "grey85")
    }
    for (co in 0:cols) {
      pc <- iso_project(c(co, co), c(0, rows), 0)
      graphics::lines(pc$sx, pc$sy, col = "grey85")
    }
  }

  ord <- order(layout$row + layout$col)
  gapmargin <- gap / 2
  for (i in ord) {
    r <- layout$row[i]
    co <- layout$col[i]
    draw_iso_bar(x0 = co - 1 + gapmargin, x1 = co - gapmargin,
                 y0 = r - 1 + gapmargin, y1 = r - gapmargin,
                 z1 = layout$value[i] * heightscale,
                 topcolor = layout$color[i], border = border,
                 shade_top = shade_top, shade_front = shade_front, shade_right = shade_right)
  }

  if (labels) {
    for (co in seq_len(cols)) {
      lp <- iso_project(co - 0.5, rows, 0)
      graphics::text(lp$sx, lp$sy - pad * 0.3, rep(rev(c("T", "C", "A", "G")), cols / 4L)[co], cex = 0.8, xpd = NA)
    }
    for (r in seq_len(rows)) {
      lp <- iso_project(cols, r - 0.5, 0)
      graphics::text(lp$sx - pad * 0.3, lp$sy, rep(c("T", "C", "A", "G"), rows / 4L)[r], cex = 0.8, xpd = NA)
    }

    graphics::text(iso_project(cols,rows/2,0)$sx - 1, iso_project(cols,rows/2,0)$sy - .8 , "Preceding base",srt = -30, cex = 0.9, xpd = NA)
    graphics::text(iso_project(cols/2,rows,0)$sx + 0.3, iso_project(cols/2,rows,0)$sy - 1.2, "Succeeding base", srt = 30, cex = 0.9, xpd = NA)
  }

  if (zlabels || !isFALSE(zsub)) {
    z0 <- iso_project(12, 0, 0)
    z1 <- iso_project(12, 0, zscale)
    graphics::lines(c(z0$sx, z1$sx), c(z0$sy, z1$sy))
    if (zlabels) {
      for (frac in c(0, 0.5, 1)) {
        zt <- iso_project(12, 0, zscale * frac)
        graphics::segments(zt$sx, zt$sy, zt$sx - pad * 0.15, zt$sy)
        graphics::text(zt$sx - pad * 0.25, zt$sy, signif(zmax * frac, 3), cex = 0.75, adj = 1, xpd = NA)
      }
    }
    if (!isFALSE(zsub)) {
      zm <- iso_project(12, 0, zscale / 2)
      graphics::text(zm$sx - pad * 1.9, zm$sy, zsub, srt = 90, cex = 0.9, xpd = NA)
    }
  }

  invisible(layout)
}
