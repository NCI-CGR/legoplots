# legoplots

Draws the 96-trinucleotide-context mutational spectrum of a sample or
signature as a "lego plot" — a 3D-style bar plot, in the layout convention
used by Mike Lawrence's lab (MGH) — as an alternative to the flat 2D
spectrum plots used by COSMIC/Sanger.

## Installation

Install the package from a local checkout:

```r
install.packages(".", repos = NULL, type = "source")
```

For development, `devtools::load_all()` also works if devtools is installed.

## Quick start

```r
library(legoplots)

sigdata <- read.table("sigdata1.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
x <- sigdata$Signature_2
names(x) <- context96$context   # e.g. "A[C>A]A" -- sigdata1.txt happens to already be in this order

legoplot3d(x, sixcolors = "mike", labels = TRUE, zsub = "Probability",
           main = "Signature 2", outfile = "sig2.png")
```

This works identically whether you run it in RStudio, from `Rscript` on a
server with no display, or inside a Docker container — `outfile` just opens
a `png()`/`pdf()`/`svg()` device (chosen from the file extension), draws,
and closes it.

## Implementation

`legoplots` projects each 3D bar onto 2D and draws it with
`graphics::polygon()`. Base R's
`png()`/`pdf()` devices need no OpenGL, no X11, no headless-Chrome shim, and
no architecture-specific binaries. They work the same on x86_64 or arm64,
locally or in a minimal container.

## API

- `context96` — the 96-context lookup table (`context`, `label`, `order`,
  `substitution`, `color_group`, ...), sourced from `sigdata1.txt`.
- `lego_palette(sixcolors)` — `"broad"` / `"sanger"` / `"mike"`, or your own
  vector of 6 hex colors (order: C>A, C>G, C>T, T>A, T>C, T>G).
- `resolve_context_values(contextdata)` — matches a named or positional
  input vector against `context96`; errors clearly on unrecognized names
  instead of silently mis-plotting.
- `compute_lego_layout(contextdata, sixcolors)` — pure data step: returns a
  data frame of grid position, value, and color per context. No graphics.
- `render_lego3d(layout, ...)` — draws a layout data frame. Options include
  `gap`, `zscale`/`zmax` (fix `zmax` across multiple calls to keep the
  vertical scale comparable when plotting several signatures side by side),
  `labels`, `zlabels`, `zsub`, `gridlines`, `main`, face-shading factors,
  and `outfile`/`width`/`height`/`res`.
- `legoplot3d(contextdata, ...)` — the convenience wrapper: layout + render
  in one call, with the combined parameter set above.

## Input format

Named vectors are strongly preferred over positional ones — position-based
input silently produces a wrong-but-plausible-looking plot if it's ever in a
slightly different order than expected, whereas a name typo raises an error.
Two naming schemes are accepted:

- Standard SBS96 notation, e.g. `"A[C>A]A"` (matches COSMIC / SigProfiler /
  MutationalPatterns) — see `context96$context`.
- `sigdata1.txt`-style labels, e.g. `"C>A|G>T_AxA"` — see `context96$label`.

An unnamed numeric vector of exactly 96 elements is still accepted, and must
be in `context96`'s row order (the standard order used throughout
`sigdata1.txt`).

## Tests

```sh
R CMD check .
```

This checks package metadata, examples, layout logic, name matching, and PNG
rendering.
