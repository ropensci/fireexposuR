# repeat test data
filepath <- "extdata/hazard.tif"
haz <- terra::rast(system.file(filepath, package = "fireexposuR"))
filepath <- "extdata/builtsimpleexamplegeom.csv"
g <- read.csv(system.file(filepath, package = "fireexposuR"))
v <- terra::vect(as.matrix(g), "polygons", crs = haz)
nb <- terra::rasterize(v, haz)
pts <- terra::spatSample(v, 20)

expnb <- fire_exp(haz, nonburnable = nb)

pts <- terra::spatSample(terra::rescale(haz, 0.8), 30, as.points = TRUE)
fires <- terra::buffer(pts, 800)

e2 <- c(46,54,496,504) * 10000
aoi <- terra::as.polygons(terra::ext(e2), crs = haz)

# tests ========================================================================

test_that("fire_exp_validate() input checks and function messages work", {
  expect_error(fire_exp_validate(2, fires),
               "`burnableexposure` must be a SpatRaster object")
  expect_error(fire_exp_validate(expnb, 2),
               "`fires` must be a SpatVector object")
  expect_error(fire_exp_validate(expnb, fires, 2),
               "`aoi` must be a SpatVector object")
})

test_that("valdiateexp() returns object with correct class", {
  expect_s3_class(fire_exp_validate(expnb, fires), "data.frame")
  expect_s3_class(fire_exp_validate(expnb, fires, plot = T), "ggplot")
})

test_that("fire_exp_validate() runs when input conditions are met", {
  expect_no_error(fire_exp_validate(expnb, fires))
  expect_no_error(fire_exp_validate(expnb, fires, aoi))
  expect_no_error(fire_exp_validate(expnb, fires, plot = T))
  expect_no_error(fire_exp_validate(expnb, fires, aoi, plot = T))
})
