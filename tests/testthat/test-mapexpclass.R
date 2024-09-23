# test data ====================================================================
set.seed(0)
e <- c(45,55,495,505) * 10000
r <- terra::rast(resolution = 100, extent = terra::ext(e))
terra::values(r) <- sample(c(0,1), terra::ncell(r), replace = TRUE)
terra::crs(r) <- "EPSG:32608"
r <- terra::sieve(r, threshold = 50, directions = 4)
haz <- terra::sieve(r, threshold = 500, directions = 4)

# input: exposure
exp <- exposure(haz)

# input: exposure, values out of range
expvals <- exp * 10

# input: aoi, too big
aoibig <- terra::as.polygons(terra::ext(e) * 1.1)

# input: aoi, no crs
filepath <- "extdata/builtsimpleexamplegeom.csv"
g <- read.csv(system.file(filepath, package = "fireexposuR"))
m <- as.matrix(g)
aoi <- terra::vect(m, "polygons")

# input, aoi, with crs
aoicrs <- aoi
terra::crs(aoicrs) <- "EPSG:32608"

# tests ========================================================================

test_that("mapexpclass() input checks and function messages work", {
  expect_condition(mapexplass(2, "loc", aoicrs)) # stopifnot() L67
  expect_condition(mapexpclass(expvals, "loc", aoicrs)) # stopifnot() L69
  expect_condition(mapexpclass(exp, "loc", 2)) # stopifnot() L71
  expect_condition(mapexpclass(exp, "loc", aoibig)) # stopifnot() L73
  expect_condition(mapexpclass(exp, "blah", aoicrs)) # matchargs() L75
  expect_condition()
})

test_that("mapexpclass() returns object with correct class", {
  expect_s3_class(mapexpclass(exp, aoi = aoicrs), "ggplot")
})

test_that("mapexpclass() runs when input conditions are met", {
  expect_no_error(mapexpcont(exp, aoi = aoicrs))
  expect_no_error(mapexpcont(exp, aoi = aoicrs))
})
