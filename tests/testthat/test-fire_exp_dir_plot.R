# repeat test data
filepath <- "extdata/hazard.tif"
haz <- terra::rast(system.file(filepath, package = "fireexposuR"))
filepath <- "extdata/polygon_geometry.csv"
g <- read.csv(system.file(filepath, package = "fireexposuR"))
v <- terra::vect(as.matrix(g), "polygons", crs = haz)
nb <- terra::rasterize(v, haz)
pts <- terra::spatSample(v, 20)
pt_wkt <- "POINT (400000 6050000)"
<<<<<<< HEAD
pt <- terra::vect(pt_wkt, crs = haz)
=======
pt <- terra::vect(point_wkt, crs = hazard)
>>>>>>> eeec67b7ecd3657f5fbebfafac24b69e631eab29


exp <- fire_exp(haz)

t_pt <- fire_exp_dir(exp, pt)
t_pol <- fire_exp_dir(exp, v)


# tests ========================================================================

test_that("fire_exp_dir_plot() input checks work", {
  expect_error(fire_exp_dir_plot(2),
               "`transects` must be a SpatVector object")
  expect_error(fire_exp_dir_plot(t_pt, title = 2),
               "`title` must be")
  expect_error(fire_exp_dir_plot(t_pt, labels = "blah"),
               "`labels` must be") # not enough
  expect_error(fire_exp_dir_plot(t_pt, labels = c("blah", "blah")),
               "`labels` must be") # not characters
})

test_that("fire_exp_dir_plot() returns objects with correct class", {
  expect_s3_class(fire_exp_dir_plot(t_pt), "ggplot")
})

test_that("fire_exp_dir_plot() runs when input conditions are met", {
  expect_no_error(fire_exp_dir_plot(t_pt))
  expect_no_error(fire_exp_dir_plot(t_pol))
  expect_no_error(fire_exp_dir_plot(t_pt, labels = c("blah", "blah", "blah")))
  expect_no_error(fire_exp_dir_plot(t_pt, title = "blah blah blah"))
})
