# test data ====================================================================
set.seed(0)
e <- c(45,55,495,505) * 10000
r <- terra::rast(resolution = 100, extent = terra::ext(e))
terra::values(r) <- sample(c(0,1), terra::ncell(r), replace = TRUE)
terra::crs(r) <- "EPSG:32608"
r <- terra::sieve(r, threshold = 50, directions = 4)
haz <- terra::sieve(r, threshold = 500, directions = 4)
# input: aoi
filepath <- "extdata/builtsimpleexamplegeom.csv"
g <- read.csv(system.file(filepath, package = "fireexposuR"))
m <- as.matrix(g)
aoi <- terra::vect(m, "polygons", crs = haz)
# input: exposure
exp <- exposure(haz)

# tests ========================================================================

test_that("summexp() input checks work", {
  expect_condition(summexp(2)) #exp: right class
  expect_condition(summexp(exp, 2)) #aoi: right class
  expect_condition(summexp(exp, aoi, "blah")) # classify: match arg
})

test_that("summexp() returns objects with correct class", {
  expect_s3_class(summexp(exp), "data.frame")
})

test_that("summexp() runs when input conditions are met", {
  expect_no_error(summexp(exp, aoi, "loc"))
  expect_no_error(summexp(exp, aoi, "lan"))
  expect_no_error(summexp(exp, classify = "lan"))
})
