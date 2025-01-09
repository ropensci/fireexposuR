# repeat test data
filepath <- "extdata/hazard.tif"
haz <- terra::rast(system.file(filepath, package = "fireexposuR"))
filepath <- "extdata/builtsimpleexamplegeom.csv"
g <- read.csv(system.file(filepath, package = "fireexposuR"))
v <- terra::vect(as.matrix(g), "polygons", crs = haz)
nb <- terra::rasterize(v, haz)
pts <- terra::spatSample(v, 3) # reduced for speed

exp <- fire_exp(haz)


m <- as.matrix(g)
m[,"x"] <- m[,"x"] +50000
aoiout <- terra::vect(m, "polygons", crs = haz)
ptsout <- terra::spatSample(aoiout, 3)

# tests ========================================================================

test_that("fire_exp_dir_multi() input checks and function messages work", {
  expect_error(fire_exp_dir_multi(2, pts),
               "`exposure` must be a SpatRaster object")
  expect_error(fire_exp_dir_multi(exp, 2),
               "`values` must be a SpatVector")
})

test_that("fire_exp_dir_multi() runs when input conditions are met", {
  expect_no_error(fire_exp_dir_multi(exp, pts, plot = T))
  expect_no_error(fire_exp_dir_multi(exp, pts))
})
