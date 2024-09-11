# generate test data ===========================================================
set.seed(0)
e <- c(45,55,495,505) * 10000
r <- terra::rast(resolution = 100, extent = terra::ext(e))
terra::values(r) <- sample(c(0,1), terra::ncell(r), replace = TRUE)
terra::crs(r) <- "EPSG:32608"
r <- terra::sieve(r, threshold = 50, directions = 4)
haz <- terra::sieve(r, threshold = 500, directions = 4)

# input: exposure
exp <- exposure(haz)
mapexpcont(exp)

# input: exposure, values out of range
expvals <- exp * 10

# input: aoi, too big
aoibig <- terra::as.polygons(terra::ext(e) * 1.1)

# input: aoi, no crs
e2 <- c(45,50,495,500) * 10000
aoi <- terra::as.polygons(terra::ext(e2))

# input, aoi, with crs
aoicrs <- aoi
terra::crs(aoicrs) <- "EPSG:32608"

test_that("mapexpcont() input checks and function messages work", {
  expect_condition(mapexpcont(2)) # stopifnot() L29
  expect_condition(mapexpcont(expvals)) # stopifnot() L31
  expect_condition(mapexpcont(exp, 2)) # stopifnot() L39
  expect_condition(mapexpcont(exp, aoibig)) # stopifnot() L41
  expect_condition(mapexpcont(exp, aoi)) # stopifnot() L43
})

test_that("mapexpcont() returns object with correct class", {
  expect_s3_class(mapexpcont(exp), "ggplot")
})

test_that("mapexpcont() runs when input conditions are met", {
  expect_no_error(mapexpcont(exp))
  expect_no_error(mapexpcont(exp, aoi = aoicrs))
})
