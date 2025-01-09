# repeat test data
filepath <- "extdata/hazard.tif"
haz <- terra::rast(system.file(filepath, package = "fireexposuR"))
filepath <- "extdata/polygon_geometry.csv"
g <- read.csv(system.file(filepath, package = "fireexposuR"))
v <- terra::vect(as.matrix(g), "polygons", crs = haz)
nb <- terra::rasterize(v, haz)
pts <- terra::spatSample(v, 20)

exp <- fire_exp(haz)

pols <- terra::buffer(pts, 100)

ext_pts <- fire_exp_extract(exp, pts)
ext_pols <- fire_exp_extract(exp, pols)

# tests ========================================================================

test_that("fire_exp_extract_vis() input checks work", {
  expect_error(fire_exp_extract_vis(2),
               "`values_ext` must be a SpatVector object")
  expect_error(fire_exp_extract_vis(pts),
               "`values_ext` missing exposure attributes")
  expect_error(fire_exp_extract_vis(ext_pols, method = "blah"),
               "'arg' should be one of")
  expect_error(fire_exp_extract_vis(ext_pts, classify = "blah"),
               "'arg' should be one of")
  expect_error(fire_exp_extract_vis(ext_pols, classify = "blah"),
               "'arg' should be one of")
})

test_that("fire_exp_extract_vis() returns objects with correct class", {
  expect_s3_class(fire_exp_extract_vis(ext_pts), "data.frame")
  expect_s3_class(fire_exp_extract_vis(ext_pts, map = T), "ggplot")
})

test_that("fire_exp_extract_vis() runs when input conditions are met", {
  expect_no_error(fire_exp_extract_vis(ext_pts))
  expect_no_error(fire_exp_extract_vis(ext_pts, map = T))
  expect_no_error(fire_exp_extract_vis(ext_pts, classify = "lan"))
  expect_no_error(fire_exp_extract_vis(ext_pts, classify = "lan", map = T))
  expect_no_error(fire_exp_extract_vis(ext_pols, method = "mean"))
  expect_no_error(fire_exp_extract_vis(ext_pols, method = "mean", map = T))
})
