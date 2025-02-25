

exp <- fire_exp(haz)


m <- as.matrix(g)
m[, "x"] <- m[, "x"] + 50000
aoiout <- terra::vect(m, "polygons", crs = haz)
ptsout <- terra::spatSample(aoiout, 3)

# tests ========================================================================

test_that("fire_exp_dir_multi() input checks and function messages work", {
  pts <- pts(n = 3) # reduced for speed
  expect_error(fire_exp_dir_multi(2, pts),
               "`exposure` must be a SpatRaster object")
  expect_error(fire_exp_dir_multi(exp, 2),
               "`values` must be a SpatVector")
})

test_that("fire_exp_dir_multi() runs when input conditions are met", {
  pts <- pts(n = 3) # reduced for speed
  expect_no_error(fire_exp_dir_multi(exp, pts, plot = TRUE, interval = 10))
  expect_no_error(fire_exp_dir_multi(exp, pts, interval = 10))
})
