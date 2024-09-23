# generate example hazard data -----------------------------
set.seed(0)
e <- c(45,55,495,505) * 10000
r <- terra::rast(resolution = 100, extent = terra::ext(e))
terra::values(r) <- sample(c(0,1), terra::ncell(r), replace = TRUE)
terra::crs(r) <- "EPSG:32608"
r <- terra::sieve(r, threshold = 50, directions = 4)
haz <- terra::sieve(r, threshold = 500, directions = 4)
exp <- exposure(haz)

filepath <- "extdata/builtsimpleexamplegeom.csv"
g <- read.csv(system.file(filepath, package = "fireexposuR"))
m <- as.matrix(g)
aoi <- terra::vect(m, "polygons", crs = haz)

pts <- terra::spatSample(aoi, 200)



test_that("multidirexp() input checks and function messages work", {
  #multidirexp()

})


test_that("multidirexp() returns object with correct class", {

})

test_that("multidirexp() runs when input conditions are met", {

})
