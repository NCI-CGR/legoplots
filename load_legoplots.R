## Source everything needed for legoplot3d(), in dependency order.
## Pure base R -- no packages to install, no rgl.
## Run this from the project directory (or open legoplots.Rproj first).
## Usage: source("load_legoplots.R")

files <- c("context96.R", "lego_layout.R", "lego_render.R", "legoplot3d_v2.R")
for (f in files) {
  source(f)
}

invisible(NULL)
