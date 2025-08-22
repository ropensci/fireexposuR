test_that("fire_exp_extract_summary() input checks work", {
  exp <- exposure()
  pts <- pts(20)
  pols <- terra::buffer(pts, 200)
  ext_pts <- fire_exp_extract(exp, pts)
  ext_pols <- fire_exp_extract(exp, pols)
  expect_error(fire_exp_extract_summary(2),
               "`values_ext` must be a SpatVector")
  expect_error(fire_exp_extract_summary(pts),
               "`values_ext` missing exposure attribute")
  expect_error(fire_exp_extract_summary(ext_pols, method = "blah"),
               "'arg' should be one of")
  expect_error(fire_exp_extract_summary(ext_pts, classify = "blah"),
               "'arg' should be one of")
  expect_error(fire_exp_extract_summary(ext_pols, classify = "blah"),
               "'arg' should be one of")
})

test_that("fire_exp_extract_summary() returns objects with correct class", {
  exp <- exposure()
  pts <- pts(20)
  ext_pts <- fire_exp_extract(exp, pts)
  expect_s3_class(fire_exp_extract_summary(ext_pts), "data.frame")
})

test_that("fire_exp_extract_summary() runs when input conditions are met", {
  exp <- exposure()
  pts <- pts(20)
  pols <- terra::buffer(pts, 200)
  ext_pts <- fire_exp_extract(exp, pts)
  ext_pols <- fire_exp_extract(exp, pols)
  expect_no_error(fire_exp_extract_summary(ext_pts))
  expect_no_error(fire_exp_extract_summary(ext_pts, classify = "lan"))
  expect_no_error(fire_exp_extract_summary(ext_pols, method = "mean"))
})
