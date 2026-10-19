#' Plot an SBS288 spectrum by transcriptional strand
#'
#' Read an SBS288 SigProfiler matrix and draw its untranscribed (U),
#' transcribed (T), and non-transcribed (N) channels as three SBS96 lego plots
#' arranged side by side. The panels use a shared vertical scale by default.
#'
#' @param contextdata An SBS288 SigProfiler matrix or data frame, or a path to
#'   a tab-delimited SBS288 matrix. Mutation types may be written as prefixed
#'   labels such as `U:A[C>A]A`, or as separate strand and context columns.
#' @param sample Sample or signature column name or number. May be omitted for
#'   a one-column matrix.
#' @param ... Additional arguments passed to [legoplot3d()], such as `colors`,
#'   `free_z`, `mar`, `main_line`, `outfile`, `width`, and `height`. The
#'   `sample`, `nrow`, and `ncol` arguments are controlled by this wrapper.
#' @return A named list containing the U, T, and N layout data frames,
#'   invisibly.
#' @examples
#' mutation_types <- unlist(lapply(
#'   c("U", "T", "N"),
#'   function(strand) paste0(strand, ":", context96$context)
#' ))
#' sbs288 <- data.frame(
#'   MutationType = mutation_types,
#'   Sample1 = seq_along(mutation_types),
#'   check.names = FALSE
#' )
#' legoplot3d_sbs288(sbs288, sample = "Sample1")
#' @export
legoplot3d_sbs288 <- function(contextdata, sample = NULL, ...) {
  sbs288 <- read_sigprofiler_sbs288(contextdata)
  values <- select_sigprofiler_sample(sbs288, sample = sample)
  strands <- c("U", "T", "N")

  strand_matrix <- vapply(strands, function(strand) {
    unname(values[paste0(strand, ":", context96$context)])
  }, numeric(nrow(context96)))
  rownames(strand_matrix) <- context96$context
  colnames(strand_matrix) <- c("Untranscribed","Transcribed","Intergenic")

  legoplot3d(
    strand_matrix,
    sample = c("Untranscribed","Transcribed","Intergenic"),
    nrow = 1L,
    ncol = 3L,
    ...
  )
}

read_sigprofiler_sbs288 <- function(x, sep = "\t") {
  if (is.character(x) && length(x) == 1L) {
    if (is.na(x) || !nzchar(x) || !file.exists(x)) {
      stop("x must name an existing SBS288 matrix file.", call. = FALSE)
    }
    x <- utils::read.table(
      x,
      header = TRUE,
      sep = sep,
      quote = "",
      comment.char = "",
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }

  if (!is.matrix(x) && !is.data.frame(x)) {
    stop("contextdata must be an SBS288 file path, matrix, or data frame.",
         call. = FALSE)
  }
  if (nrow(x) == 0L || ncol(x) == 0L) {
    stop("The SBS288 matrix must contain rows and columns.", call. = FALSE)
  }

  tab <- as.data.frame(x, check.names = FALSE, stringsAsFactors = FALSE)

  # Accept transposed matrices whose mutation types are columns.
  column_labels <- parse_sbs288_labels(names(tab))
  if (sum(column_labels$valid) == 288L) {
    context_idx <- which(column_labels$valid)
    sample_idx <- setdiff(seq_along(tab), context_idx)
    sample_names <- rownames(tab)
    if (length(sample_idx) == 1L && !is.numeric(tab[[sample_idx]])) {
      sample_names <- as.character(tab[[sample_idx]])
    }
    values <- t(coerce_sigprofiler_values(tab[context_idx]))
    colnames(values) <- sample_names
    return(order_sbs288_matrix(
      values,
      strand = column_labels$strand[context_idx],
      context = column_labels$context[context_idx]
    ))
  }

  strand <- NULL
  contexts <- NULL
  metadata <- integer()

  # Prefer a column containing complete strand-prefixed mutation labels.
  for (i in seq_along(tab)) {
    parsed <- parse_sbs288_labels(tab[[i]])
    if (all(parsed$valid)) {
      strand <- parsed$strand
      contexts <- parsed$context
      metadata <- i
      break
    }
  }

  # Otherwise identify separate strand and SBS96 context columns.
  if (is.null(contexts)) {
    strand_idx <- which(vapply(tab, function(z) {
      values <- trimws(as.character(z))
      length(values) > 0L && all(values %in% c("U", "T", "N"))
    }, logical(1)))
    context_idx <- which(vapply(tab, function(z) {
      values <- canonical_sigprofiler_context(z)
      length(values) > 0L && all(values %in% context96$context)
    }, logical(1)))

    if (length(strand_idx) && length(context_idx)) {
      strand_idx <- strand_idx[1L]
      context_idx <- context_idx[1L]
      strand <- trimws(as.character(tab[[strand_idx]]))
      contexts <- canonical_sigprofiler_context(tab[[context_idx]])
      metadata <- c(strand_idx, context_idx)
    }
  }

  # Support files with separate strand, substitution, and trinucleotide fields.
  if (is.null(contexts)) {
    strand_idx <- which(vapply(tab, function(z) {
      values <- trimws(as.character(z))
      length(values) > 0L && all(values %in% c("U", "T", "N"))
    }, logical(1)))
    substitution_idx <- which(vapply(tab, function(z) {
      values <- trimws(as.character(z))
      length(values) > 0L && all(grepl("^[CT]>[ACGT]$", values))
    }, logical(1)))
    trinucleotide_idx <- which(vapply(tab, function(z) {
      values <- trimws(as.character(z))
      length(values) > 0L && all(grepl("^[ACGT]{3}$", values))
    }, logical(1)))

    if (length(strand_idx) && length(substitution_idx) &&
        length(trinucleotide_idx)) {
      strand_idx <- strand_idx[1L]
      substitution_idx <- substitution_idx[1L]
      trinucleotide_idx <- trinucleotide_idx[1L]
      strand <- trimws(as.character(tab[[strand_idx]]))
      substitution <- trimws(as.character(tab[[substitution_idx]]))
      trinucleotide <- trimws(as.character(tab[[trinucleotide_idx]]))
      contexts <- paste0(
        substr(trinucleotide, 1L, 1L), "[", substitution, "]",
        substr(trinucleotide, 3L, 3L)
      )
      metadata <- c(strand_idx, substitution_idx, trinucleotide_idx)
    }
  }

  # Numeric matrices commonly store prefixed mutation labels as row names.
  if (is.null(contexts) && !is.null(rownames(tab))) {
    parsed <- parse_sbs288_labels(rownames(tab))
    if (all(parsed$valid)) {
      strand <- parsed$strand
      contexts <- parsed$context
    }
  }

  if (is.null(contexts)) {
    stop(
      "Could not identify U, T, and N SBS288 mutation types. ",
      "Expected labels such as U:A[C>A]A.",
      call. = FALSE
    )
  }

  value_idx <- setdiff(seq_along(tab), metadata)
  numeric_idx <- value_idx[vapply(tab[value_idx], is_sigprofiler_numeric,
                                  logical(1))]
  if (!length(numeric_idx)) {
    stop("No numeric sample or signature columns were found.", call. = FALSE)
  }
  values <- coerce_sigprofiler_values(tab[numeric_idx])
  order_sbs288_matrix(values, strand = strand, context = contexts)
}

parse_sbs288_labels <- function(x) {
  labels <- trimws(as.character(x))
  prefixed <- grepl("^[UTN]:", labels)
  strand <- ifelse(prefixed, substr(labels, 1L, 1L), NA_character_)
  context <- ifelse(prefixed, substring(labels, 3L), NA_character_)
  valid <- prefixed & context %in% context96$context
  list(strand = strand, context = context, valid = valid)
}

order_sbs288_matrix <- function(x, strand, context) {
  if (length(strand) != nrow(x) || length(context) != nrow(x) ||
      any(!strand %in% c("U", "T", "N")) ||
      any(!context %in% context96$context)) {
    stop("The matrix contains mutation types outside SBS288 U/T/N contexts.",
         call. = FALSE)
  }

  keys <- paste0(strand, ":", context)
  collapsed <- rowsum(x, group = keys, reorder = FALSE)
  expected <- unlist(lapply(
    c("U", "T", "N"),
    function(s) paste0(s, ":", context96$context)
  ), use.names = FALSE)
  result <- matrix(
    0,
    nrow = length(expected),
    ncol = ncol(collapsed),
    dimnames = list(expected, colnames(collapsed))
  )
  result[rownames(collapsed), ] <- collapsed

  missing <- setdiff(expected, rownames(collapsed))
  if (length(missing)) {
    warning(length(missing), " SBS288 context(s) were absent and filled with 0.",
            call. = FALSE)
  }
  result
}
