#' Canonical table of the 96 trinucleotide substitution contexts
#'
#' Context identities in the row order used by the 12x8 plotting grid.
#'
#' @format A data frame with 96 rows and columns:
#' \describe{
#'   \item{order}{Position in the plotting grid.}
#'   \item{substitution}{Pyrimidine-based substitution class.}
#'   \item{preceding, succeeding}{Flanking bases.}
#'   \item{context}{Standard SBS96 trinucleotide notation.}
#'   \item{label}{Expanded label, e.g. "C>A|G>T_AxA".}
#' }
#' @export
context96 <- local({

  substitution_classes <- c("C>T|G>A", "T>C|A>G", "C>A|G>T", "T>G|A>C", "C>G|G>C", "T>A|A>T")

  tab <- structure(list(order = 1:96, substitution = structure(c(1L, 1L, 
  1L, 1L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 1L, 
  1L, 1L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 
  3L, 3L, 4L, 4L, 4L, 4L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 3L, 3L, 
  3L, 3L, 4L, 4L, 4L, 4L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 5L, 5L, 
  5L, 5L, 6L, 6L, 6L, 6L, 5L, 5L, 5L, 5L, 6L, 6L, 6L, 6L, 5L, 5L, 
  5L, 5L, 6L, 6L, 6L, 6L, 5L, 5L, 5L, 5L, 6L, 6L, 6L, 6L), levels = c("C>T|G>A", 
  "T>C|A>G", "C>A|G>T", "T>G|A>C", "C>G|G>C", "T>A|A>T"), class = "factor"), 
      preceding = structure(c(1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 
      2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 
      1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 
      4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 
      3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 
      2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 
      1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L), levels = c("T", 
      "C", "A", "G"), class = "factor"), succeeding = structure(c(4L, 
      4L, 4L, 4L, 4L, 4L, 4L, 4L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 
      2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
      1L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 3L, 3L, 3L, 3L, 3L, 3L, 
      3L, 3L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 
      1L, 1L, 1L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 3L, 3L, 3L, 3L, 
      3L, 3L, 3L, 3L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 
      1L, 1L, 1L, 1L, 1L), levels = c("T", "C", "A", "G"), class = "factor")), row.names = c(NA, 
  -96L), class = "data.frame")

  # Add the two accepted context-name formats.
  tab$context <- paste0(tab$preceding, "[", sub("\\|.*", "", tab$substitution), "]", tab$succeeding)
  tab$label <- paste0(tab$substitution, "_", tab$preceding, "x", tab$succeeding)
  tab
})
