## Static base-R renderer for lego plots.

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
#' Project grid coordinates using the requested camera angles.
#'
#' @param x,y,z Numeric vectors of grid/height coordinates.
#' @param elevation Camera elevation in degrees.
#' @param azimuth Camera rotation around the plot in degrees.
#' @return A list with elements \code{sx}, \code{sy} (screen coordinates).
#' @keywords internal
iso_project <- function(x, y, z = 0, elevation = 35.26439, azimuth = 45) {
  # Convert camera angles to radians.
  elevation <- elevation * pi / 180
  azimuth <- azimuth * pi / 180

  # Project while keeping z measured in grid units.
  list(
    sx = (y * cos(azimuth) - x * sin(azimuth)) / cos(elevation),
    sy = -(x * cos(azimuth) + y * sin(azimuth)) * tan(elevation) + z
  )
}

#' Draw one 3D bar as three shaded isometric faces
#'
#' Draw the two visible sides and top of one bar.
#'
#' @param x0,x1,y0,y1 Footprint of the bar in grid units.
#' @param z1 Bar height in grid units (base is always at \code{z = 0}).
#' @param topcolor Base color of the bar; the two side faces are automatically
#'   shaded darker/lighter versions of it.
#' @param alpha Bar opacity between 0 (fully transparent) and 1 (opaque).
#' @param elevation Camera elevation in degrees.
#' @param azimuth Camera rotation around the plot in degrees.
#' @param border Outline color for each face, or \code{NA} for none.
#' @param shade_top,shade_front,shade_right Brightness multipliers (see \code{\link{shade_color}}).
#' @keywords internal
draw_iso_bar <- function(x0, x1, y0, y1, z1, topcolor, alpha = 1,
                          elevation = 35.26439, azimuth = 45,
                          border = "grey15",
                          shade_top = 1, shade_front = 0.8, shade_right = 0.6) {
  # Project and draw one face.
  quad <- function(x, y, z, col) {
    p <- iso_project(x, y, z, elevation = elevation, azimuth = azimuth)
    graphics::polygon(p$sx, p$sy,
                      col = grDevices::adjustcolor(col, alpha.f = alpha),
                      border = border, lwd = 0.6)
  }
  if (z1 <= 0) return(invisible())

  # Draw the right, front, and top faces.
  quad(c(x1, x1, x1, x1), c(y0, y1, y1, y0), c(0, 0, z1, z1),
       shade_color(topcolor, shade_right))
  quad(c(x0, x1, x1, x0), c(y1, y1, y1, y1), c(0, 0, z1, z1),
       shade_color(topcolor, shade_front))
  quad(c(x0, x1, x1, x0), c(y0, y0, y1, y1), c(z1, z1, z1, z1),
       shade_color(topcolor, shade_top))
  invisible()
}

#' Render a lego layout
#'
#' Draw a static projected 3D plot using base graphics.
#'
#' @param layout A data frame as returned by \code{\link{compute_lego_layout}}.
#' @param gap Fraction (0-1) of each grid cell left empty as a gap between bars.
#' @param zscale Height in grid units of the value at \code{zmax}.
#' @param zmax Data value mapped to \code{zscale}; defaults to the maximum value.
#' @param labels Draw "preceding base" / "succeeding base" axis labels.
#' @param zlabels Draw a numeric scale on the vertical (z) axis.
#' @param zsub Descriptive label for the vertical axis, or \code{FALSE} for none.
#' @param gridlines Draw a floor grid under the bars.
#' @param main Optional plot title.
#' @param legend Draw a boxed base-substitution color legend.
#' @param alpha Bar opacity between 0 (fully transparent) and 1 (opaque).
#' @param elevation Camera elevation in degrees, greater than 0 and less than 90.
#' @param azimuth Camera rotation around the plot in degrees, from 10 to 80.
#' @param shade_top,shade_front,shade_right Face brightness multipliers.
#' @param border Outline color for bar faces.
#' @param outfile Output path, or \code{NA} for the current graphics device.
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
                           gridlines = TRUE, main = NULL, legend = FALSE, alpha = 1,
                           elevation = 35.26439, azimuth = 45,
                           shade_top = 1, shade_front = 0.8, shade_right = 0.6,
                           border = "grey15",
                           outfile = NA, width = 1000, height = 800, res = 150) {

  # Validate view and transparency options.
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) ||
      alpha < 0 || alpha > 1) {
    stop("alpha must be a single number between 0 and 1.", call. = FALSE)
  }
  if (!is.logical(legend) || length(legend) != 1L || is.na(legend)) {
    stop("legend must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(elevation) || length(elevation) != 1L || !is.finite(elevation) ||
      elevation <= 0 || elevation >= 90) {
    stop("elevation must be a single number greater than 0 and less than 90 degrees.",
         call. = FALSE)
  }
  if (!is.numeric(azimuth) || length(azimuth) != 1L || !is.finite(azimuth) ||
      azimuth < 10 || azimuth > 80) {
    stop("azimuth must be a single number between 10 and 80 degrees.", call. = FALSE)
  }

  # Determine grid dimensions and vertical scaling.
  rows <- max(layout$row)
  cols <- max(layout$col)

  if (is.null(zmax)) zmax <- max(layout$value, na.rm = TRUE)
  heightscale <- if (isTRUE(zmax > 0)) zscale / zmax else 0

  # Open an output device when a file is requested.
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

  # Set up a canvas large enough for the projected grid and bars.
  corners <- expand.grid(x = c(0, cols), y = c(0, rows), z = c(0, zscale))
  p <- iso_project(corners$x, corners$y, corners$z,
                   elevation = elevation, azimuth = azimuth)
  pad <- 0.08 * max(diff(range(p$sx)), diff(range(p$sy)))
  graphics::par(mar = c(1, 1, if (is.null(main)) 1 else 3, 1))
  graphics::plot(NA, xlim = range(p$sx) + c(-pad, pad) * 2.2,
                 ylim = range(p$sy) + c(-pad, pad * 1.3),
                 asp = 1, axes = FALSE, xlab = "", ylab = "")
  if (!is.null(main)) graphics::title(main = main)

  # Draw the floor grid.
  if (gridlines) {
    for (r in 0:rows) {
      pr <- iso_project(c(0, cols), c(r, r), 0,
                        elevation = elevation, azimuth = azimuth)
      graphics::lines(pr$sx, pr$sy, col = "grey85")
    }
    for (co in 0:cols) {
      pc <- iso_project(c(co, co), c(0, rows), 0,
                        elevation = elevation, azimuth = azimuth)
      graphics::lines(pc$sx, pc$sy, col = "grey85")
    }
  }

  # Draw bars from back to front for correct overlap.
  azimuth_rad <- azimuth * pi / 180
  ord <- order(layout$col * cos(azimuth_rad) + layout$row * sin(azimuth_rad))
  gapmargin <- gap / 2
  for (i in ord) {
    r <- layout$row[i]
    co <- layout$col[i]
    draw_iso_bar(x0 = co - 1 + gapmargin, x1 = co - gapmargin,
                 y0 = r - 1 + gapmargin, y1 = r - gapmargin,
                 z1 = layout$value[i] * heightscale,
                 topcolor = layout$color[i], alpha = alpha,
                 elevation = elevation, azimuth = azimuth, border = border,
                 shade_top = shade_top, shade_front = shade_front, shade_right = shade_right)
  }

  # Draw base labels along the projected axes.
  if (labels) {
    elevation_rad <- elevation * pi / 180
    axis_x <- c(-sin(azimuth_rad) / cos(elevation_rad),
                -cos(azimuth_rad) * tan(elevation_rad))
    axis_y <- c(cos(azimuth_rad) / cos(elevation_rad),
                -sin(azimuth_rad) * tan(elevation_rad))
    unit_x <- axis_x / sqrt(sum(axis_x^2))
    unit_y <- axis_y / sqrt(sum(axis_y^2))
    angle_x <- atan2(-axis_x[2], -axis_x[1]) * 180 / pi
    angle_y <- atan2(axis_y[2], axis_y[1]) * 180 / pi

    for (co in seq_len(cols)) {
      lp <- iso_project(co - 0.5, rows, 0,
                        elevation = elevation, azimuth = azimuth)
      graphics::text(lp$sx + unit_y[1] * pad * 0.3,
                     lp$sy + unit_y[2] * pad * 0.3,
                     rep(rev(c("T", "C", "A", "G")), cols / 4L)[co],
                     cex = 0.8, xpd = NA)
    }
    for (r in seq_len(rows)) {
      lp <- iso_project(cols, r - 0.5, 0,
                        elevation = elevation, azimuth = azimuth)
      graphics::text(lp$sx + unit_x[1] * pad * 0.3,
                     lp$sy + unit_x[2] * pad * 0.3,
                     rep(c("T", "C", "A", "G"), rows / 4L)[r],
                     cex = 0.8, xpd = NA)
    }

    preceding <- iso_project(cols, rows / 2, 0,
                             elevation = elevation, azimuth = azimuth)
    succeeding <- iso_project(cols / 2, rows, 0,
                              elevation = elevation, azimuth = azimuth)
    graphics::text(preceding$sx + unit_x[1] * pad * 0.9,
                   preceding$sy + unit_x[2] * pad * 0.9,
                   "Preceding base", srt = angle_y, cex = 0.9, xpd = NA)
    graphics::text(succeeding$sx + unit_y[1] * pad * 0.9,
                   succeeding$sy + unit_y[2] * pad * 0.9,
                   "Succeeding base", srt = angle_x, cex = 0.9, xpd = NA)
  }

  # Draw the vertical scale and title.
  if (zlabels || !isFALSE(zsub)) {
    z0 <- iso_project(cols, 0, 0, elevation = elevation, azimuth = azimuth)
    z1 <- iso_project(cols, 0, zscale, elevation = elevation, azimuth = azimuth)
    graphics::lines(c(z0$sx, z1$sx), c(z0$sy, z1$sy))
    if (zlabels) {
      for (frac in c(0, 0.5, 1)) {
        zt <- iso_project(cols, 0, zscale * frac,
                          elevation = elevation, azimuth = azimuth)
        graphics::segments(zt$sx, zt$sy, zt$sx - pad * 0.15, zt$sy)
        graphics::text(zt$sx - pad * 0.25, zt$sy, signif(zmax * frac, 3), cex = 0.75, adj = 1, xpd = NA)
      }
    }
    if (!isFALSE(zsub)) {
      zm <- iso_project(cols, 0, zscale / 2,
                        elevation = elevation, azimuth = azimuth)
      graphics::text(zm$sx - pad * 1.9, zm$sy, zsub, srt = 90, cex = 0.9, xpd = NA)
    }
  }

  # Draw the substitution-color legend.
  if (legend) {
    classes <- c("C>T", "C>A", "C>G", "T>C", "T>A", "T>G")
    reverse_classes <- c("G>A", "G>T", "G>C","A>G", "A>T", "A>C")
    class_colors <- layout$color[match(classes, substr(layout$substitution, 1, 3))]
    graphics::legend(
      "topright",
      legend = paste(classes, "or", reverse_classes),
      fill = class_colors,
      border = "grey15",
      bg = "white",
      title = "Base Substitutions",
      cex = 0.9,
      inset = 0.02
    )
  }

  invisible(layout)
}
