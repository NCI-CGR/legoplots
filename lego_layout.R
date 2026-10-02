#' Six-color palettes for lego plots
#'
#' Return six colors ordered C>A, C>G, C>T, T>A, T>C, T>G.
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
  # Define substitution classes and built-in palettes.
  substitution_classes <- c("C>A", "C>G", "C>T", "T>A", "T>C", "T>G")
  presets <- list(
    sanger = c("#55BAE9", "#000000", "#D23E32", "#CCC9CA", "#ABCD70", "#E6C7C6"),
    broad  = c("#547CE3", "#805D3F", "#FFFF54", "#794EAC", "#5FC941", "#1232C4"),
    mike   = c("#4FAFB1", "#EA3323", "#FFFF54", "#794EAC", "#5FC941", "#547CE3")
  )
  
  # Resolve a preset name or validate a custom palette.
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

#' Resolve context values into plotting order
#'
#' Match named SBS96 values to \code{context96}; unnamed values must already
#' follow \code{context96} row order.
#'
#' @param contextdata A numeric vector of length 96, optionally named.
#' @return A numeric vector of length 96 in plotting order.
#' @export
resolve_context_values <- function(contextdata) {
  # Validate the input type and determine whether names are available.
  if (!is.numeric(contextdata)) {
    stop("contextdata must be numeric.", call. = FALSE)
  }

  nm <- names(contextdata)
  named <- !is.null(nm) && all(nzchar(nm))

  # Accept positional input only when all 96 contexts are supplied.
  if (!named) {
    if (length(contextdata) != 96) {
      stop("Unnamed contextdata must have exactly 96 elements, in the standard order ",
           "documented in context96$context (see context96.R). ",
           "To avoid relying on positional order, name your vector instead, e.g. ",
           "names(x) <- context96$context.", call. = FALSE)
    }
    return(as.numeric(contextdata))
  }

  # Match either SBS96 names or expanded labels.
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

  # Fill missing named contexts with zero.
  values <- numeric(96)
  values[idx] <- as.numeric(contextdata)
  values
}

#' Compute the bar layout for a mutational-context lego plot
#'
#' Assign each context its grid position, value, and color.
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
  # Resolve input values and palette colors.
  cols <- 8L
  values <- resolve_context_values(contextdata)
  palette <- lego_palette(sixcolors)

  # Attach values, colors, and grid coordinates to the context table.
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
