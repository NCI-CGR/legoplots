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

  # Standard orthographic projection. Avoid dividing by cos(elevation), which
  # would apply an elevation-dependent zoom as the camera moves.
  list(
    sx = y * cos(azimuth) - x * sin(azimuth),
    sy = -(x * cos(azimuth) + y * sin(azimuth)) * sin(elevation) +
      z * cos(elevation)
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
#' @param width,height Output size in pixels. PDF/SVG dimensions in inches are
#'   calculated as `width / res` and `height / res`.
#' @param res Resolution in dpi, used for PNG output.
#' @return The \code{layout} data frame, invisibly.
#' @examples
#' layout <- compute_lego_layout(setNames(runif(96), context96$context))
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

  # Validate the layout before using it to establish graphics coordinates.
  required <- c("row", "col", "substitution", "value", "color")
  if (!is.data.frame(layout) || !nrow(layout) ||
      !all(required %in% names(layout))) {
    stop("layout must be a non-empty data frame returned by compute_lego_layout().",
         call. = FALSE)
  }
  if (!is.numeric(layout$row) || !is.numeric(layout$col) ||
      any(!is.finite(layout$row)) || any(!is.finite(layout$col)) ||
      any(layout$row < 1 | layout$row != as.integer(layout$row)) ||
      any(layout$col < 1 | layout$col != as.integer(layout$col))) {
    stop("layout row and col values must be positive integers.", call. = FALSE)
  }
  if (!is.numeric(layout$value) || any(!is.finite(layout$value)) ||
      any(layout$value < 0)) {
    stop("layout values must be finite, non-negative numbers.", call. = FALSE)
  }
  valid_layout_colors <- tryCatch({
    grDevices::col2rgb(layout$color)
    TRUE
  }, error = function(e) FALSE)
  if (!valid_layout_colors) {
    stop("layout contains an invalid color.", call. = FALSE)
  }

  # Validate view, scaling, and transparency options.
  if (!is.numeric(gap) || length(gap) != 1L || !is.finite(gap) ||
      gap < 0 || gap >= 1) {
    stop("gap must be a single number greater than or equal to 0 and less than 1.",
         call. = FALSE)
  }
  if (!is.numeric(zscale) || length(zscale) != 1L || !is.finite(zscale) ||
      zscale <= 0) {
    stop("zscale must be a single positive number.", call. = FALSE)
  }
  if (!is.null(zmax) && (!is.numeric(zmax) || length(zmax) != 1L ||
      !is.finite(zmax) || zmax <= 0)) {
    stop("zmax must be NULL or a single positive number.", call. = FALSE)
  }
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) ||
      alpha < 0 || alpha > 1) {
    stop("alpha must be a single number between 0 and 1.", call. = FALSE)
  }
  if (!is.logical(legend) || length(legend) != 1L || is.na(legend)) {
    stop("legend must be TRUE or FALSE.", call. = FALSE)
  }
  logical_options <- list(labels = labels, zlabels = zlabels, gridlines = gridlines)
  invalid_logical <- !vapply(logical_options, function(x) {
    is.logical(x) && length(x) == 1L && !is.na(x)
  }, logical(1))
  if (any(invalid_logical)) {
    stop(paste(names(logical_options)[invalid_logical], collapse = ", "),
         " must be TRUE or FALSE.", call. = FALSE)
  }
  if (!(identical(zsub, FALSE) ||
        (is.character(zsub) && length(zsub) == 1L && !is.na(zsub)))) {
    stop("zsub must be FALSE or a single character string.", call. = FALSE)
  }
  if (!(is.null(main) ||
        (is.character(main) && length(main) == 1L && !is.na(main)))) {
    stop("main must be NULL or a single character string.", call. = FALSE)
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
  shades <- c(shade_top = shade_top, shade_front = shade_front,
              shade_right = shade_right)
  if (!is.numeric(shades) || any(!is.finite(shades)) || any(shades < 0)) {
    stop("Face-shading factors must be finite, non-negative numbers.", call. = FALSE)
  }
  if (length(border) != 1L || (!is.na(border) && !is.character(border))) {
    stop("border must be NA or one valid R color.", call. = FALSE)
  }
  if (!is.na(border)) {
    valid_border <- tryCatch({
      grDevices::col2rgb(border)
      TRUE
    }, error = function(e) FALSE)
    if (!valid_border) stop("border must be NA or one valid R color.", call. = FALSE)
  }
  dimensions <- c(width = width, height = height, res = res)
  if (!is.numeric(dimensions) || any(!is.finite(dimensions)) ||
      any(dimensions <= 0)) {
    stop("width, height, and res must be positive numbers.", call. = FALSE)
  }
  use_current_device <- length(outfile) == 1L && is.na(outfile)
  if (!use_current_device &&
      (!is.character(outfile) || length(outfile) != 1L || !nzchar(outfile))) {
    stop("outfile must be NA or one non-empty file path.", call. = FALSE)
  }
  # Determine grid dimensions and vertical scaling.
  rows <- max(layout$row)
  cols <- max(layout$col)

  if (is.null(zmax)) zmax <- max(layout$value, na.rm = TRUE)
  heightscale <- if (isTRUE(zmax > 0)) zscale / zmax else 0

  # Open an output device when a file is requested.
  if (!use_current_device) {
    ext <- tolower(tools::file_ext(outfile))
    if (!ext %in% c("png", "pdf", "svg")) {
      stop("outfile must use a .png, .pdf, or .svg extension.", call. = FALSE)
    }
    dev_open <- switch(ext,
      pdf = function() grDevices::pdf(outfile, width = width / res, height = height / res),
      svg = function() grDevices::svg(outfile, width = width / res, height = height / res),
      function() grDevices::png(outfile, width = width, height = height, res = res)
    )
    previous_device <- grDevices::dev.cur()
    dev_open()
    if (identical(grDevices::dev.cur(), previous_device)) {
      stop("The ", toupper(ext), " graphics device could not be opened.", call. = FALSE)
    }
    on.exit(grDevices::dev.off(), add = TRUE)
  } else {
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
  }

  # Set up a canvas large enough for the projected grid and bars.
  plotted_zmax <- max(zscale, layout$value * heightscale)
  corners <- expand.grid(x = c(0, cols), y = c(0, rows), z = c(0, plotted_zmax))
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
    axis_x <- c(-sin(azimuth_rad),
                -cos(azimuth_rad) * sin(elevation_rad))
    axis_y <- c(cos(azimuth_rad),
                -sin(azimuth_rad) * sin(elevation_rad))
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
