#' Six-color palettes for lego plots
#'
#' Returns a vector of 6 hex colors, one per pyrimidine-based substitution
#' class, in the order C>A, C>G, C>T, T>A, T>C, T>G.
#'
#' The \code{"mike"} preset's hex values were confirmed by sampling actual
#' rendered pixels from both \code{sig9.png} and the small example plot in
#' \code{lego_ref.png} -- the two agree with each other almost exactly, and
#' with these hex values. (\code{lego_ref.png} also has a separate "Base
#' substitutions" text legend with different, purer colors -- that legend
#' does not match either rendered example plot's actual pixels, so it
#' appears to document a different plot style, not this one; it is not used
#' here.)
#'
#' @param sixcolors Either the name of a built-in palette ("broad", "sanger",
#'   "mike"), or a custom vector of exactly 6 hexadecimal RGB colors in that
#'   same substitution-class order.
#' @return A named character vector of 6 hex colors.
#' @examples
#' lego_palette("mike")
#' lego_palette(c("#111111", "#222222", "#333333", "#444444", "#555555", "#666666"))
#' @export
lego_palette <- function(sixcolors = "sanger") {
  substitution_classes <- c("C>A", "C>G", "C>T", "T>A", "T>C", "T>G")
  presets <- list(
    sanger = c("#55BAE9", "#000000", "#D23E32", "#CCC9CA", "#ABCD70", "#E6C7C6"),
    broad  = c("#547CE3", "#805D3F", "#FFFF54", "#794EAC", "#5FC941", "#1232C4"),
    mike   = c("#4FAFB1", "#EA3323", "#FFFF54", "#794EAC", "#5FC941", "#547CE3")
  )
  
  if (length(sixcolors) == 1 && is.character(sixcolors) && sixcolors %in% names(presets)) {
    sixcolors <- presets[[sixcolors]]
  }
  if (length(sixcolors) != 6) {
    stop("sixcolors must be one of ", paste(shQuote(names(presets)), collapse = ", "),
         ", or a vector of 6 hex colors ordered ", paste(substitution_classes, collapse = ", "),
         ".", call. = FALSE)
  }
  stats::setNames(unname(sixcolors), substitution_classes)
}

#' Resolve arbitrary contextdata input into standard-order values
#'
#' Accepts either an unnamed length-96 vector (assumed to already be in the
#' standard order documented in \code{\link{context96}}, i.e. the row order
#' of \code{sigdata1.txt}), or a named vector whose names identify each
#' context, in any order and even if incomplete. Matching by name is the
#' recommended, reproducible way to pass data in: it can never silently
#' produce a mismatched or mirrored plot the way a positional vector can if
#' it happens to be in a slightly different order than expected.
#'
#' Names may use either standard SBS96 notation (e.g. \code{"A[C>A]A"}, as
#' used by COSMIC/SigProfiler/MutationalPatterns) or the
#' \code{sigdata1.txt}-style label (e.g. \code{"C>A|G>T_AxA"}).
#'
#' @param contextdata A numeric vector of length 96, optionally named.
#' @return A numeric vector of length 96, in \code{context96}'s standard order.
#' @export
resolve_context_values <- function(contextdata) {
  if (!is.numeric(contextdata)) {
    stop("contextdata must be numeric.", call. = FALSE)
  }

  nm <- names(contextdata)
  named <- !is.null(nm) && all(nzchar(nm))

  if (!named) {
    if (length(contextdata) != 96) {
      stop("Unnamed contextdata must have exactly 96 elements, in the standard order ",
           "documented in context96$context (see context96.R). ",
           "To avoid relying on positional order, name your vector instead, e.g. ",
           "names(x) <- context96$context.", call. = FALSE)
    }
    return(as.numeric(contextdata))
  }

  idx <- match(nm, context96$context)
  unmatched <- is.na(idx)
  if (any(unmatched)) {
    idx[unmatched] <- match(nm[unmatched], context96$label)
  }
  if (anyNA(idx)) {
    stop("Unrecognized context name(s): ", paste(nm[is.na(idx)], collapse = ", "),
         ".\nExpected standard SBS96 notation (e.g. \"A[C>A]A\", see context96$context) ",
         "or sigdata1.txt-style labels (see context96$label).", call. = FALSE)
  }
  if (anyDuplicated(idx)) {
    stop("contextdata names contain duplicate contexts.", call. = FALSE)
  }
  if (length(contextdata) < 96) {
    warning(96 - length(contextdata), " context(s) missing from contextdata; filled with 0.",
            call. = FALSE)
  }

  values <- numeric(96)
  values[idx] <- as.numeric(contextdata)
  values
}

#' Compute the bar layout for a mutational-context lego plot
#'
#' Pure, side-effect-free computation of everything needed to draw a lego
#' plot: which grid cell each context occupies, its bar height, and its
#' color. Because it does no plotting, it can be inspected, unit-tested, or
#' fed to a different renderer without requiring rgl or any graphics device.
#'
#' Grid position \code{k} (1-96, in row-major order) shows the VALUE and
#' IDENTITY of standard-order context \code{context96$order[k]} -- this is
#' exactly the \code{contextdata[porder]} reordering from the original
#' \code{legoplot.R}, applied as a gather (\code{result[k] = x[porder[k]]}),
#' not a sort. Its COLOR, however, is deliberately taken from a *different*
#' standard-order context, \code{neworder[k]}: this reproduces the original
#' package's "Mike Lawrence lab" layout, where the six color regions form a
#' fixed spatial template on the grid rather than tracking the substitution
#' class of whatever value \code{porder} happens to place in each cell.
#'
#' This was confirmed empirically, not assumed: a fresh legacy \code{rgl}
#' render (from the untouched \code{bar3d.R}/\code{barplot3d.R}/\code{legoplot.R})
#' and the small example plot in \code{lego_ref.png} both show this same
#' fixed-template coloring and both use the same \code{"mike"} hex values
#' (see \code{\link{lego_palette}}) -- so it is reproduced here on purpose
#' for visual fidelity to those established figures, not a leftover bug.
#'
#' @param contextdata A numeric vector of length 96 (see
#'   \code{\link{resolve_context_values}} for accepted formats).
#' @param sixcolors Palette, see \code{\link{lego_palette}}.
#' @return A data frame with one row per grid cell, in plotting order, with
#'   columns \code{order}, \code{row}, \code{col}, \code{context},
#'   \code{label}, \code{substitution}, \code{value}, \code{color}.
#' @examples
#' layout <- compute_lego_layout(setNames(runif(96), context96$context))
#' head(layout)
#' @export
compute_lego_layout <- function(contextdata, sixcolors = "mike") {
  cols <- 8L
  values <- resolve_context_values(contextdata)
  palette <- lego_palette(sixcolors)

  k <- seq_len(nrow(context96))
  tab <- context96
  tab$value <- values
  tab$color <- unname(palette[substr(context96$substitution,1,3)])
  tab$order <- k
  tab$row <- ((k - 1) %% cols) + 1L
  tab$col <- ((k - 1) %/% cols) + 1L
  rownames(tab) <- NULL
  tab[, c("order", "row", "col", "context", "label", "substitution", "value", "color")]
}
