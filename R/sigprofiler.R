#' Read an SBS96 matrix produced by SigProfiler
#'
#' Import an SBS96 matrix produced by SigProfilerMatrixGenerator,
#' SigProfilerExtractor, or another SigProfiler Suite tool. Mutation contexts
#' may be stored in a `MutationType` or `Mutation Types` column, in row names,
#' or as 96 matrix columns. Files with separate substitution and trinucleotide
#' columns are also supported. Strand prefixes such as `T:` and `U:` are
#' removed and duplicate contexts are summed to SBS96.
#'
#' @param x Path to a tab-delimited SigProfiler matrix, or an in-memory matrix
#'   or data frame.
#' @param sep Field separator used when `x` is a path.
#' @return A numeric matrix with SBS96 contexts as row names and samples or
#'   signatures as columns, ordered to match [context96].
#' @examples
#' sigprofiler <- data.frame(
#'   MutationType = context96$context,
#'   Sample1 = seq_len(96),
#'   check.names = FALSE
#' )
#' matrix <- read_sigprofiler_sbs96(sigprofiler)
#' head(matrix)
#' @export
read_sigprofiler_sbs96 <- function(x, sep = "\t") {
  if (is.character(x) && length(x) == 1L) {
    if (is.na(x) || !nzchar(x) || !file.exists(x)) {
      stop("x must name an existing SigProfiler matrix file.", call. = FALSE)
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
    stop("x must be a file path, matrix, or data frame.", call. = FALSE)
  }
  if (nrow(x) == 0L || ncol(x) == 0L) {
    stop("The SigProfiler matrix must contain rows and columns.", call. = FALSE)
  }

  tab <- as.data.frame(x, check.names = FALSE, stringsAsFactors = FALSE)

  # Accept transposed matrices whose 96 contexts are columns.
  column_contexts <- canonical_sigprofiler_context(names(tab))
  context_columns <- match(column_contexts, context96$context)
  if (sum(!is.na(context_columns)) == 96L) {
    context_idx <- which(!is.na(context_columns))
    sample_idx <- setdiff(seq_along(tab), context_idx)
    sample_names <- rownames(tab)
    if (length(sample_idx) == 1L && !is.numeric(tab[[sample_idx]])) {
      sample_names <- as.character(tab[[sample_idx]])
    }
    values <- coerce_sigprofiler_values(tab[context_idx])
    values <- t(values)
    rownames(values) <- column_contexts[context_idx]
    colnames(values) <- sample_names
    return(order_sigprofiler_matrix(values))
  }

  contexts <- NULL
  metadata <- integer()

  # Prefer a column whose contents are complete SBS96 context labels.
  for (i in seq_along(tab)) {
    candidate <- canonical_sigprofiler_context(tab[[i]])
    if (all(candidate %in% context96$context)) {
      contexts <- candidate
      metadata <- i
      break
    }
  }

  # Some MatrixGenerator outputs separate the substitution and trinucleotide.
  if (is.null(contexts)) {
    substitution_idx <- which(vapply(tab, function(z) {
      values <- trimws(as.character(z))
      all(grepl("^[CT]>[ACGT]$", values))
    }, logical(1)))
    trinucleotide_idx <- which(vapply(tab, function(z) {
      values <- trimws(as.character(z))
      all(grepl("^[ACGT]{3}$", values))
    }, logical(1)))

    if (length(substitution_idx) && length(trinucleotide_idx)) {
      substitution_idx <- substitution_idx[1L]
      trinucleotide_idx <- trinucleotide_idx[1L]
      substitution <- trimws(as.character(tab[[substitution_idx]]))
      trinucleotide <- trimws(as.character(tab[[trinucleotide_idx]]))
      contexts <- paste0(
        substr(trinucleotide, 1L, 1L), "[", substitution, "]",
        substr(trinucleotide, 3L, 3L)
      )
      metadata <- c(substitution_idx, trinucleotide_idx)
    }
  }

  # Numeric matrices commonly store contexts as row names.
  if (is.null(contexts) && !is.null(rownames(tab))) {
    candidate <- canonical_sigprofiler_context(rownames(tab))
    if (all(candidate %in% context96$context)) {
      contexts <- candidate
    }
  }

  if (is.null(contexts) || any(!contexts %in% context96$context)) {
    stop(
      "Could not identify SBS96 mutation contexts in the SigProfiler matrix. ",
      "Expected labels such as A[C>A]A.",
      call. = FALSE
    )
  }

  value_idx <- setdiff(seq_along(tab), metadata)
  numeric_idx <- value_idx[vapply(tab[value_idx], is_sigprofiler_numeric, logical(1))]
  if (!length(numeric_idx)) {
    stop("No numeric sample or signature columns were found.", call. = FALSE)
  }
  values <- coerce_sigprofiler_values(tab[numeric_idx])
  rownames(values) <- contexts
  order_sigprofiler_matrix(values)
}

canonical_sigprofiler_context <- function(x) {
  x <- trimws(as.character(x))
  sub("^[TUNBQ]:", "", x)
}

is_sigprofiler_numeric <- function(x) {
  values <- suppressWarnings(as.numeric(as.character(x)))
  length(values) > 0L && all(is.finite(values))
}

coerce_sigprofiler_values <- function(x) {
  columns <- lapply(x, function(column) {
    suppressWarnings(as.numeric(as.character(column)))
  })
  values <- matrix(
    unlist(columns, use.names = FALSE),
    nrow = nrow(x),
    ncol = ncol(x),
    dimnames = list(NULL, names(x))
  )
  if (any(!is.finite(values))) {
    stop("SigProfiler values must not contain NA, NaN, or infinite values.", call. = FALSE)
  }
  if (any(values < 0)) {
    stop("SigProfiler values must not be negative.", call. = FALSE)
  }
  values
}

order_sigprofiler_matrix <- function(x) {
  contexts <- canonical_sigprofiler_context(rownames(x))
  if (any(!contexts %in% context96$context)) {
    stop("The matrix contains contexts outside SBS96.", call. = FALSE)
  }

  collapsed <- rowsum(x, group = contexts, reorder = FALSE)
  result <- matrix(
    0,
    nrow = nrow(context96),
    ncol = ncol(collapsed),
    dimnames = list(context96$context, colnames(collapsed))
  )
  result[rownames(collapsed), ] <- collapsed

  missing <- setdiff(context96$context, rownames(collapsed))
  if (length(missing)) {
    warning(length(missing), " SBS96 context(s) were absent and filled with 0.",
            call. = FALSE)
  }
  result
}

select_sigprofiler_sample <- function(x, sample = NULL) {
  if (is.null(sample)) {
    if (ncol(x) != 1L) {
      stop(
        "The SigProfiler matrix contains multiple columns; select one with sample =. ",
        "Available columns: ", paste(colnames(x), collapse = ", "), ".",
        call. = FALSE
      )
    }
    sample <- 1L
  }

  if (is.character(sample) && length(sample) == 1L && !is.na(sample)) {
    sample <- match(sample, colnames(x))
    if (is.na(sample)) {
      stop("Unknown sample or signature name.", call. = FALSE)
    }
  } else if (is.numeric(sample) && length(sample) == 1L && is.finite(sample) &&
             sample == as.integer(sample) && sample >= 1L && sample <= ncol(x)) {
    sample <- as.integer(sample)
  } else {
    stop("sample must be one column name or column number.", call. = FALSE)
  }

  stats::setNames(as.numeric(x[, sample]), rownames(x))
}
