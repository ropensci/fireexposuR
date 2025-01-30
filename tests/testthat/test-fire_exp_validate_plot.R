# repeat test data
filepath <- "extdata/hazard.tif"
haz <- terra::rast(system.file(filepath, package = "fireexposuR"))
filepath <- "extdata/polygon_geometry.csv"
g <- read.csv(system.file(filepath, package = "fireexposuR"))
v <- terra::vect(as.matrix(g), "polygons", crs = haz)
nb <- terra::rasterize(v, haz)
pts <- terra::spatSample(v, 20)

expnb <- fire_exp(haz, no_burn = nb)

pts <- terra::spatSample(terra::rescale(haz, 0.8), 30, as.points = TRUE)
fires <- terra::buffer(pts, 800)

e <- c(39, 40, 604, 605) * 10000
aoi <- terra::as.polygons(terra::ext(e), crs = haz)

set.seed(0)
output1 <- fire_exp_validate(expnb, fires)

set.seed(1)
output2 <- fire_exp_validate(expnb, fires)

set.seed(0)
output3 <- fire_exp_validate(expnb, fires)

# tests ========================================================================

test_that("fire_exp_valdiate_plot() returns object with correct class", {
  expect_s3_class(fire_exp_validate_plot(output1), "ggplot")
})
