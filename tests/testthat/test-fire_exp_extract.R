# repeat test data
filepath <- "extdata/hazard.tif"
haz <- terra::rast(system.file(filepath, package = "fireexposuR"))
filepath <- "extdata/builtsimpleexamplegeom.csv"
g <- read.csv(system.file(filepath, package = "fireexposuR"))
v <- terra::vect(as.matrix(g), "polygons", crs = haz)
nb <- terra::rasterize(v, haz)
pts <- terra::spatSample(v, 20)

exp <- fire_exp(haz)

pols <- terra::buffer(pts, 100)

# tests ========================================================================

test_that("fire_exp_extract() input checks work", {
  expect_error(fire_exp_extract(2),
               "`exposure` must be a SpatRaster object")
  expect_error(fire_exp_extract(exp, 2),
               "`values` must be a SpatVector object")
})

test_that("fire_exp_extract() returns objects with correct class", {
  expect_s4_class(fire_exp_extract(exp, pts), "SpatVector")
  expect_s4_class(fire_exp_extract(exp, pols), "SpatVector")
})

test_that("fire_exp_extract() runs when input conditions are met", {
  expect_no_error(fire_exp_extract(exp, pols))
  expect_no_error(fire_exp_extract(exp, pts))
})
