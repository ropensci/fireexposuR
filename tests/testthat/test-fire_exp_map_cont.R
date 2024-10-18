# repeat test data
filepath <- "extdata/hazard.tif"
haz <- terra::rast(system.file(filepath, package = "fireexposuR"))
filepath <- "extdata/builtsimpleexamplegeom.csv"
g <- read.csv(system.file(filepath, package = "fireexposuR"))
v <- terra::vect(as.matrix(g), "polygons", crs = haz)
nb <- terra::rasterize(v, haz)
pts <- terra::spatSample(v, 20)

exp <- fire_exp(haz)

cropexp <- terra::crop(exp, terra::rescale(v,0.5))

nocrs <- exp
terra::crs(nocrs) <- ""

# tests ========================================================================

test_that("fire_exp_map_cont() input checks and function messages work", {
  expect_condition(fire_exp_map_cont(2),
                   "`exposure` must be a SpatRaster object")
  expect_condition(fire_exp_map_cont(exp * 2),
                   "`exposure` layer must have values between 0-1")
  expect_condition(fire_exp_map_cont(exp, 2),
                   "`aoi` must be a SpatVector object")
  expect_condition(fire_exp_map_cont(cropexp, v),
                   "`aoi` extent must be within `exposure` extent")
  expect_condition(fire_exp_map_cont(nocrs, v),
                   "`exposure` and `aoi` must have same CRS")
})

test_that("fire_exp_map_cont() returns object with correct class", {
  expect_s3_class(suppressMessages(fire_exp_map_cont(exp)), "ggplot")
})

test_that("fire_exp_map_cont() runs when input conditions are met", {
  expect_no_error(suppressMessages(fire_exp_map_cont(exp)))
  expect_no_error(suppressMessages(fire_exp_map_cont(exp, v)))
})
