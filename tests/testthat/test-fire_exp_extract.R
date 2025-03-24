test_that("fire_exp_extract() input checks work", {
  exp <- exposure()
  pts <- pts(20)
  pols <- terra::buffer(pts, 200)
  expect_error(fire_exp_extract(2),
               "`exposure` must be a SpatRaster object")
  expect_error(fire_exp_extract(exp, 2),
               "`values` must be a SpatVector object")
})

test_that("fire_exp_extract() returns objects with correct class", {
  exp <- exposure()
  pts <- pts(20)
  pols <- terra::buffer(pts, 200)
  expect_s4_class(fire_exp_extract(exp, pts), "SpatVector")
  expect_s4_class(fire_exp_extract(exp, pols), "SpatVector")
})

test_that("fire_exp_extract() runs when input conditions are met", {
  exp <- exposure()
  pts <- pts(20)
  pols <- terra::buffer(pts, 200)
  expect_no_error(fire_exp_extract(exp, pols))
  expect_no_error(fire_exp_extract(exp, pts))
})
