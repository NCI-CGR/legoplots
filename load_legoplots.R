## Backward-compatible loader for users of the original script collection.
## Installing the package and calling library(legoplots) is preferred.

files <- c("context96.R", "lego_layout.R", "lego_render.R", "sigprofiler.R",
           "legoplot3d.R")
for (f in files) source(file.path("R", f))

invisible(NULL)
