# test data ====================================================================
set.seed(0)
e <- c(45,55,495,505) * 10000
r <- terra::rast(resolution = 100, extent = terra::ext(e))
terra::values(r) <- sample(c(0,1), terra::ncell(r), replace = TRUE)
terra::crs(r) <- "EPSG:32608"
r <- terra::sieve(r, threshold = 50, directions = 4)
haz <- terra::sieve(r, threshold = 500, directions = 4)
# input : nonburnable, with no CRS
nb <- terra::rast(resolution = 100, extent = terra::ext(e))
terra::values(nb) <- suppressWarnings(sample(c(NA, 1), terra::ncell(nb),
                                             replace = TRUE, prob = c(0.9,0.1)))
# input: nonburnable, with CRS
nbcrs <- nb
terra::crs(nbcrs) <- "EPSG:32608"

expnb <- exposure(haz, nonburnable = nbcrs)

filepath <- "extdata/builtsimpleexamplegeom.csv"
g <- read.csv(system.file(filepath, package = "fireexposuR"))
m <- as.matrix(g)
fires <- terra::vect(m, "polygons", crs = haz)

e2 <- c(46,54,496,504) * 10000
aoi <- terra::as.polygons(terra::ext(e2), crs = haz)

# tests ========================================================================

test_that("validateexp() input checks and function messages work", {
  expect_error(validateexp(2, fires))
  expect_error(validateexp(expnb, 2))
  expect_error(validateexp(expnb, fires, 2))
})

test_that("valdiateexp() returns object with correct class", {
  expect_s3_class(validateexp(expnb, fires), "data.frame")
  expect_s3_class(validateexp(expnb, fires, plot = T), "ggplot")
})

test_that("validateexp() runs when input conditions are met", {
  expect_no_error(validateexp(expnb, fires))
  expect_no_error(validateexp(expnb, fires, aoi))
  expect_no_error(validateexp(expnb, fires, plot = T))
  expect_no_error(validateexp(expnb, fires, aoi, plot = T))
})
