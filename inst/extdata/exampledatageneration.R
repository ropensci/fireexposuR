# generate example hazard data -----------------------------
set.seed(0)
e <- c(45, 55, 495, 505) * 10000
r <- terra::rast(resolution = 100, extent = terra::ext(e))
terra::values(r) <- sample(c(0, 1), terra::ncell(r), replace = TRUE)
terra::crs(r) <- "EPSG:32608"
r <- terra::sieve(r, threshold = 50, directions = 4)
haz <- terra::sieve(r, threshold = 500, directions = 4)
# generate an example point ---------------------------------
wkt <- "POINT (500000 5000000)"
pt <- terra::vect(wkt, crs = haz)
# generate example AOI polygon -----------------------------
filepath <- "extdata/builtsimpleexamplegeom.csv"
g <- read.csv(system.file(filepath, package = "fireexposuR"))
m <- as.matrix(g)
v <- terra::vect(m, "polygons", crs = haz)
# generate example point values within polygon -------------
pts <- terra::spatSample(v, 200)
# or example points across the landscape -------------------
e <- terra::buffer(terra::vect(terra::ext(haz), crs = haz), -15500)
pts <- terra::spatSample(e, 200)
# ----------------------------------------------------------
