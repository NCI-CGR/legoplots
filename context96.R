#' Canonical table of the 96 trinucleotide substitution contexts
#'
#' A lookup table describing every context used by \code{\link{legoplot3d}}.
#' Row order matches the "standard" 96-context order used throughout the
#' mutational-signatures literature (COSMIC/SigProfiler convention, and the
#' row order of \code{sigdata1.txt}): the six pyrimidine-based substitution
#' classes (C>A|G>T, C>G|G>C, C>T|G>A, T>A|A>T, T>C|A>G, T>G|A>C), each split
#' into 16 contexts ordered by succeeding base (A,C,G,T) then preceding base
#' (A,C,G,T).
#'
#' The \code{order} column is copied verbatim from the \code{order} column of
#' \code{sigdata1.txt} and is the ONLY source of truth for where a context is
#' drawn in the 12x8 "lego" grid (this is what previously lived as the opaque
#' \code{porder}/\code{neworder} magic-number vectors). Because both the bar
#' height and the bar color are looked up from the same row of this table,
#' a context's data and its color can never fall out of sync, regardless of
#' what order the caller's data arrives in.
#'
#' @format A data frame with 96 rows and columns:
#' \describe{
#'   \item{standard_order}{Position (1-96) in the standard/default input order.}
#'   \item{order}{Position (1-96) in the 12x8 "lego" plotting grid (Mike Lawrence lab layout).}
#'   \item{substitution}{One of the six pyrimidine-based substitution classes, e.g. "C>A|G>T".}
#'   \item{preceding, succeeding}{Flanking bases.}
#'   \item{context}{Standard SBS96 notation, e.g. "A[C>A]A" (matches COSMIC/SigProfiler/MutationalPatterns).}
#'   \item{label}{sigdata1.txt-style label, e.g. "C>A|G>T_AxA".}
#'   \item{color_group}{Index 1-6 into a six-color palette, one per substitution class.}
#' }
#' @source \code{sigdata1.txt}, columns \code{order}, \code{Substitution_type},
#'   \code{Preceeding_base}, \code{Succeeding_base}.
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

  tab$context <- paste0(tab$preceding, "[", sub("\\|.*", "", tab$substitution), "]", tab$succeeding)
  tab$label <- paste0(tab$substitution, "_", tab$preceding, "x", tab$succeeding)
  tab
})
