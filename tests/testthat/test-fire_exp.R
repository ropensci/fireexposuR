# repeat test data
filepath <- "extdata/hazard.tif"
haz <- terra::rast(system.file(filepath, package = "fireexposuR"))
filepath <- "extdata/builtsimpleexamplegeom.csv"
g <- read.csv(system.file(filepath, package = "fireexposuR"))
v <- terra::vect(as.matrix(g), "polygons", crs = haz)
nb <- terra::rasterize(v, haz)
pts <- terra::spatSample(v, 20)

# function specific test data
nocrsh <- haz
terra::crs(nocrsh) <- ""

nocrsnb <- nb
terra::crs(nocrsnb) <- ""

smallhaz <- terra::crop(haz,terra::rescale(v,0.5), mask = TRUE)

# tests ========================================================================

test_that("fire_exp() input checks and function messages work", {
  expect_error(fire_exp(5),
               "`hazard` must be a SpatRaster object")
  expect_error(fire_exp(haz * 2),
               "`hazard` layer must have values between 0-1")
  expect_error(fire_exp(haz, tdist = "x"),
               "'arg' should be one of")
  expect_message(fire_exp(nocrsh),
                 "Input CRS is undefined:")
  expect_error(fire_exp(terra::rescale(haz, 2)),
               "Insufficient resolution for l")
  expect_error(fire_exp(haz, tdist = "s"),
               "Insufficient resolution for short")
  expect_error(fire_exp(haz, tdist = "r"),
               "Insufficient resolution for rad")
  expect_error(fire_exp(smallhaz),
               "Extent of hazard raster too small for exposure assessment")
  expect_error(fire_exp(haz, nonburnable = 5),
               "`nonburnable` must be a SpatRaster")
  expect_error(fire_exp(haz, nonburnable = nocrsnb),
               "nonburnable` and `hazard` must have same CRS")
  expect_error(fire_exp(haz, nonburnable = nb * 2),
               "must only contain values of 1 or NA")
  expect_error(fire_exp(haz, nonburnable = terra::extend(nb, 50, fill = 1)),
               "extent must be within `hazard` extent")
})

test_that("fire_exp() returns object with correct class", {
  expect_s4_class(fire_exp(haz), "SpatRaster")
  expect_s4_class(fire_exp(haz, nonburnable = nb), "SpatRaster")
})

test_that("fire_exp() runs when input conditions are met", {
  expect_no_condition(fire_exp(haz))
  expect_no_condition(fire_exp(haz * 0.5))
  expect_no_condition(fire_exp(haz, nonburnable = nb))
})
