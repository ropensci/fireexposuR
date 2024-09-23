# generate example hazard data -----------------------------
set.seed(0)
e <- c(45,55,495,505) * 10000
r <- terra::rast(resolution = 100, extent = terra::ext(e))
terra::values(r) <- sample(c(0,1), terra::ncell(r), replace = TRUE)
terra::crs(r) <- "EPSG:32608"
r <- terra::sieve(r, threshold = 50, directions = 4)
haz <- terra::sieve(r, threshold = 500, directions = 4)
# generate an example point ---------------------------------
wkt <- "POINT (500000 5000000)"
pt <- terra::vect(wkt, crs = haz)
# -----------------------------------------------------------
exp <- exposure(haz)

filepath <- "extdata/builtsimpleexamplegeom.csv"
g <- read.csv(system.file(filepath, package = "fireexposuR"))
m <- as.matrix(g)
aoi <- terra::vect(m, "polygons", crs = haz)

pts <- terra::spatSample(aoi, 200)


test_that("direxp() input checks and function messages work", {
  expect_error(direxp(2, pt))
  expect_error(direxp(haz, 2))
  expect_error(direxp(haz, pt, plot = T, map = T))
  expect_message(direxp(haz, pts))
})


test_that("direxp() returns object with correct class", {
  expect_s4_class(direxp(exp, pt), "SpatVector")
  expect_s3_class(direxp(exp, pt, map = T), "ggplot")
  expect_s3_class(direxp(exp, pt, plot = T), "ggplot")
  expect_s3_class(direxp(exp, pt, table = T), "data.frame")
})

test_that("direxp() runs when input conditions are met", {
  expect_no_error(direxp(exp, pt))
  expect_no_error(direxp(exp, pts))
  expect_no_error(direxp(exp, aoi))
  expect_no_error(direxp(exp, pt, map = TRUE))
  expect_no_error(direxp(exp, pts, map = TRUE))
  expect_no_error(direxp(exp, aoi, map = TRUE))
  expect_no_error(direxp(exp, pt, plot = TRUE))
  expect_no_error(direxp(exp, pts, plot = TRUE))
  expect_no_error(direxp(exp, aoi, plot = TRUE))
  expect_no_error(direxp(exp, pt, table = TRUE))
  expect_no_error(direxp(exp, pts, table = TRUE))
  expect_no_error(direxp(exp, aoi, table = TRUE))
})
