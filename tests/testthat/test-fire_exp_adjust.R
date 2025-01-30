# repeat test data
filepath <- "extdata/hazard.tif"
haz <- terra::rast(system.file(filepath, package = "fireexposuR"))
filepath <- "extdata/polygon_geometry.csv"
g <- read.csv(system.file(filepath, package = "fireexposuR"))
v <- terra::vect(as.matrix(g), "polygons", crs = haz)
nb <- terra::rasterize(v, haz)
pts <- terra::spatSample(v, 20)


# tests ========================================================================

test_that("fire_exp_adjust() input checks work", {
  expect_error(fire_exp_adjust(2),
               "`hazard` must be a SpatRaster object")
  expect_error(fire_exp_adjust(haz),
               "is missing, with no default")
  expect_error(fire_exp_adjust(haz * 2, 300),
               "`hazard` layer must have values between 0-1")
  expect_error(fire_exp_adjust(haz, "l"),
               "`tdist` must be numeric")
  expect_error(fire_exp_adjust(haz, 50),
               "insufficient resolution")
  expect_error(fire_exp_adjust(haz, 350, 4),
               "`no_burn` must be a SpatRaster object")
})

test_that("fire_exp_adjust() returns correct object class", {
  expect_s4_class(fire_exp_adjust(haz, 350), "SpatRaster")
})

test_that("fire_exp_adjust() runs when input conditions are met", {
  expect_no_error(fire_exp_adjust(haz, 350))
  expect_no_error(fire_exp_adjust(haz, 350, nb))
})
