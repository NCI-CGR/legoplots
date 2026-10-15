library(legoplots)

expect_error <- function(expr, pattern = NULL) {
  error <- tryCatch({
    force(expr)
    NULL
  }, error = identity)
  stopifnot(inherits(error, "error"))
  if (!is.null(pattern)) stopifnot(grepl(pattern, conditionMessage(error)))
  invisible(error)
}

stopifnot(
  is.data.frame(context96),
  nrow(context96) == 96L,
  identical(lego_palette(), lego_palette("sanger")),
  identical(formals(legoplot3d)$colors, "sanger"),
  identical(formals(animate_legoplot3d)$colors, "sanger"),
  identical(names(lego_palette("mike")),
            c("C>A", "C>G", "C>T", "T>A", "T>C", "T>G"))
)
expect_error(lego_palette(c(rep("black", 5), "not-a-color")), "invalid")

values <- stats::setNames(seq_len(96) / 96, context96$context)
layout <- compute_lego_layout(values)

stopifnot(
  nrow(layout) == 96L,
  identical(layout$value, unname(values)),
  all(layout$row %in% seq_len(8L)),
  all(layout$col %in% seq_len(12L))
)

shuffled <- values[rev(seq_along(values))]
stopifnot(identical(compute_lego_layout(shuffled)$value, unname(values)))

partial_names <- values
names(partial_names)[1] <- ""
expect_error(resolve_context_values(partial_names), "completely named")
expect_error(resolve_context_values(replace(values, 1, NA_real_)), "must not contain")
expect_error(resolve_context_values(replace(values, 1, -1)), "negative")
expect_error(resolve_context_values(stats::setNames(values, rep(context96$context[1], 96))),
             "duplicate")

# SigProfiler context-by-sample format, including file input.
sigprofiler <- data.frame(
  `Mutation Types` = context96$context,
  Sample1 = seq_len(96),
  Sample2 = seq_len(96) * 2,
  check.names = FALSE
)
sigprofiler_matrix <- read_sigprofiler_sbs96(sigprofiler)
stopifnot(
  identical(dim(sigprofiler_matrix), c(96L, 2L)),
  identical(rownames(sigprofiler_matrix), context96$context),
  identical(unname(sigprofiler_matrix[, "Sample1"]), as.numeric(seq_len(96)))
)

sigprofiler_file <- tempfile(fileext = ".txt")
utils::write.table(sigprofiler, sigprofiler_file, sep = "\t", row.names = FALSE,
                   quote = FALSE)
stopifnot(identical(read_sigprofiler_sbs96(sigprofiler_file), sigprofiler_matrix))
expect_error(legoplot3d(sigprofiler_matrix), "multiple columns")

# Separate mutation/trinucleotide fields used by some MatrixGenerator outputs.
split_format <- data.frame(
  MutationType = sub(".*\\[([CT]>[ACGT])\\].*", "\\1", context96$context),
  Trinucleotide = paste0(substr(context96$context, 1, 1),
                         substr(context96$context, 3, 3),
                         substr(context96$context, 7, 7)),
  Sample1 = seq_len(96),
  check.names = FALSE
)
stopifnot(identical(read_sigprofiler_sbs96(split_format),
                    sigprofiler_matrix[, "Sample1", drop = FALSE]))

# Strand-aware contexts are collapsed by summing strands.
strand_format <- data.frame(
  MutationType = c(paste0("T:", context96$context), paste0("U:", context96$context)),
  Sample1 = c(rep(1, 96), rep(2, 96)),
  check.names = FALSE
)
strand_matrix <- read_sigprofiler_sbs96(strand_format)
stopifnot(all(strand_matrix[, 1] == 3))

# A transposed matrix with samples in rows is accepted.
transposed <- as.data.frame(t(sigprofiler_matrix), check.names = FALSE)
stopifnot(identical(read_sigprofiler_sbs96(transposed), sigprofiler_matrix))

# Raster and vector devices all produce non-empty files.
png_file <- tempfile(fileext = ".png")
pdf_file <- tempfile(fileext = ".pdf")
svg_file <- tempfile(fileext = ".svg")
result <- legoplot3d(values, outfile = png_file, width = 500, height = 400)
legoplot3d(sigprofiler_matrix, sample = "Sample1", outfile = pdf_file,
           width = 500, height = 400)
stopifnot(
  file.exists(png_file), file.info(png_file)$size > 0,
  file.exists(pdf_file), file.info(pdf_file)$size > 0,
  rawToChar(readBin(pdf_file, "raw", n = 4L)) == "%PDF",
  nrow(result) == 96L
)
svg_available <- tryCatch({
  suppressWarnings(legoplot3d(values, outfile = svg_file, width = 500, height = 400))
  file.exists(svg_file)
}, error = function(e) FALSE)
if (svg_available) {
  stopifnot(any(grepl("<svg", readLines(svg_file, n = 10L, warn = FALSE))))
}
expect_error(legoplot3d(values, outfile = tempfile(fileext = ".jpg")), "extension")

# Plotting on the current device does not change its graphics parameters.
device_file <- tempfile(fileext = ".pdf")
grDevices::pdf(device_file)
old_mar <- graphics::par("mar")
legoplot3d(values)
stopifnot(identical(graphics::par("mar"), old_mar))
grDevices::dev.off()

# Animation path and argument validation do not require a GIF encoder.
camera <- legoplots:::orbit_camera_path(8L, 45, 35, 15, 7)
stopifnot(
  identical(dim(camera), c(8L, 2L)),
  max(camera$azimuth) <= 60,
  min(camera$azimuth) >= 30,
  max(camera$elevation) <= 42,
  min(camera$elevation) >= 28
)

# World-z lines and bar edges remain parallel at every animation angle.
for (i in seq_len(nrow(camera))) {
  vertical <- legoplots:::iso_project(
    x = c(4, 4), y = c(6, 6), z = c(0, 3),
    elevation = camera$elevation[i], azimuth = camera$azimuth[i]
  )
  stopifnot(
    abs(diff(vertical$sx)) < sqrt(.Machine$double.eps),
    abs(diff(vertical$sy) - 3 * cos(camera$elevation[i] * pi / 180)) <
      sqrt(.Machine$double.eps)
  )
}

expect_error(
  animate_legoplot3d(values, outfile = tempfile(fileext = ".png")),
  "gif extension"
)

gif_file <- tempfile(fileext = ".gif")
if (requireNamespace("gifski", quietly = TRUE)) {
  animate_legoplot3d(
    values,
    outfile = gif_file,
    n_frames = 3L,
    fps = 3,
    width = 300,
    height = 240,
    res = 72,
    progress = FALSE
  )
  stopifnot(
    file.exists(gif_file),
    file.info(gif_file)$size > 0,
    rawToChar(readBin(gif_file, "raw", n = 6L)) %in% c("GIF87a", "GIF89a")
  )
}

unlink(c(sigprofiler_file, png_file, pdf_file, svg_file, device_file, gif_file))
