# repeat test data
filepath <- "extdata/hazard.tif"
haz <- terra::rast(system.file(filepath, package = "fireexposuR"))
filepath <- "extdata/builtsimpleexamplegeom.csv"
g <- read.csv(system.file(filepath, package = "fireexposuR"))
v <- terra::vect(as.matrix(g), "polygons", crs = haz)
nb <- terra::rasterize(v, haz)
pts <- terra::spatSample(v, 20)

exp <- fire_exp(haz)
wkt <- "POINT (500000 5000000)"
pt <- terra::vect(wkt, crs = haz)


# tests ========================================================================

test_that("fire_exp_dir() input checks and function messages work", {
  expect_error(fire_exp_dir(2, pt),
               "`exposure` must be a SpatRaster object")
  expect_error(fire_exp_dir(exp, 2),
               "`value` must be a SpatVector object")
  expect_message(fire_exp_dir(exp, pts),
                 "Value object provided has more than one feature")
})


test_that("fire_exp_dir() returns object with correct class", {
  expect_s4_class(fire_exp_dir(exp, pt), "SpatVector")
  expect_s3_class(fire_exp_dir(exp, pt, table = T), "data.frame")
})

test_that("fire_exp_dir() runs when input conditions are met", {
  expect_no_error(fire_exp_dir(exp, pt))
  expect_no_error(suppressMessages(fire_exp_dir(exp, pts)))
  expect_no_error(fire_exp_dir(exp, v))
  expect_no_error(fire_exp_dir(exp, pt, table = TRUE))
  expect_no_error(suppressMessages(fire_exp_dir(exp, pts, table = TRUE)))
  expect_no_error(fire_exp_dir(exp, v, table = TRUE))
})
