# repeat test data
filepath <- "extdata/hazard.tif"
haz <- terra::rast(system.file(filepath, package = "fireexposuR"))
filepath <- "extdata/builtsimpleexamplegeom.csv"
g <- read.csv(system.file(filepath, package = "fireexposuR"))
v <- terra::vect(as.matrix(g), "polygons", crs = haz)
nb <- terra::rasterize(v, haz)
pts <- terra::spatSample(v, 20)

exp <- fire_exp(haz)

# tests ========================================================================

test_that("fire_exp_summary() input checks work", {
  expect_condition(fire_exp_summary(2),
                   "`exposure` must be a SpatRaster object")
  expect_condition(fire_exp_summary(exp, 2),
                   "`aoi` must be a SpatVector object")
  expect_condition(fire_exp_summary(exp, v, "blah"),
                   "'arg' should be one of")
})

test_that("fire_exp_summary() returns objects with correct class", {
  expect_s3_class(fire_exp_summary(exp, v), "data.frame")
})

test_that("fire_exp_summary() runs when input conditions are met", {
  expect_no_error(fire_exp_summary(exp, v, "loc"))
  expect_no_error(fire_exp_summary(exp, v, "lan"))
  expect_no_error(fire_exp_summary(exp, classify = "lan"))
})
