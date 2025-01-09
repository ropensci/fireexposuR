# repeat test data
filepath <- "extdata/hazard.tif"
haz <- terra::rast(system.file(filepath, package = "fireexposuR"))
filepath <- "extdata/polygon_geometry.csv"
g <- read.csv(system.file(filepath, package = "fireexposuR"))
v <- terra::vect(as.matrix(g), "polygons", crs = haz)
nb <- terra::rasterize(v, haz)
pts <- terra::spatSample(v, 20)

exp <- fire_exp(haz)

cropexp <- terra::crop(exp, terra::rescale(v,0.5))

nocrs <- exp
terra::crs(nocrs) <- ""

# tests ========================================================================

test_that("fire_exp_map_class() input checks and function messages work", {
  expect_error(fire_exp_map_class(2, "loc", v),
                   "`exposure` must be a SpatRaster object")
  expect_error(fire_exp_map_class(exp * 2, "loc", v),
                   "must have values between")
  expect_error(fire_exp_map_class(exp, "loc", 2),
                   "`aoi` must be a SpatVector")
  expect_error(fire_exp_map_class(cropexp, "loc", v),
                   "`aoi` extent must be within `exposure` extent")
  expect_error(fire_exp_map_class(exp, "blah", v),
                   "'arg' should be one of")
  expect_error(fire_exp_map_class(nocrs, "lan", v),
                   "`exposure` and `aoi` must have same CRS")
})

test_that("fire_exp_map_class() returns object with correct class", {
  expect_s3_class(fire_exp_map_class(exp, aoi = v), "ggplot")
})

test_that("fire_exp_map_class() runs when input conditions are met", {
  expect_no_error(fire_exp_map_class(exp, "lan", v))
  expect_no_error(fire_exp_map_class(exp, "loc", v))
})
