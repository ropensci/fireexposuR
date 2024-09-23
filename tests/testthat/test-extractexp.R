# test data ====================================================================
set.seed(0)
e <- c(45,55,495,505) * 10000
r <- terra::rast(resolution = 100, extent = terra::ext(e))
terra::values(r) <- sample(c(0,1), terra::ncell(r), replace = TRUE)
terra::crs(r) <- "EPSG:32608"
r <- terra::sieve(r, threshold = 50, directions = 4)
haz <- terra::sieve(r, threshold = 500, directions = 4)

filepath <- "extdata/builtsimpleexamplegeom.csv"
g <- read.csv(system.file(filepath, package = "fireexposuR"))
m <- as.matrix(g)
aoi <- terra::vect(m, "polygons", crs = haz)

pts <- terra::spatSample(aoi, 200)
exp <- exposure(haz)

# tests ========================================================================

test_that("extractexp() input checks work", {
  expect_error(extractexp(2))
  expect_error(extractexp(exp, 2))
  expect_error(extractexp(haz, pts, summary = T, map = T))
  expect_error(extractexp(haz, pts, summary = T))
})

test_that("extractexp() returns objects with correct class", {
  expect_s3_class(extractexp(haz, pts, summary = T, classify = "loc"), "data.frame")
  expect_s4_class(extractexp(haz, pts), "SpatVector")
  expect_s3_class(extractexp(haz, pts, map = T, classify = "loc"), "ggplot")
})

test_that("extractexp() runs when input conditions are met", {
  expect_no_error(extractexp(haz, pts, summary = T, classify = "loc"))
  expect_no_error(extractexp(haz, pts, summary = T, classify = "lan"))
  expect_no_error(extractexp(haz, pts, map = T, classify = "loc"))
  expect_no_error(extractexp(haz, pts, map = T, classify = "lan"))
  expect_no_error(extractexp(haz, pts, classify = "loc"))
  expect_no_error(extractexp(haz, pts, classify = "lan"))
  expect_no_error(extractexp(haz, pts))
})
