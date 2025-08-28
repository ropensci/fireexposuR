test_that("fire_exp_extract_vis() input checks work", {
  exp <- exposure()
  pts <- pts(10)
  pols <- terra::buffer(pts, 200)
  ext_pts <- fire_exp_extract(exp, pts)
  ext_pols <- fire_exp_extract(exp, pols)

  expect_warning(fire_exp_extract_vis(ext_pts),
                 "'fire_exp_extract_vis' is deprecated")
  expect_error(fire_exp_extract_vis(2),
               "`values_ext` must be a SpatVector")
  expect_error(fire_exp_extract_vis(pts),
               "`values_ext` missing exposure attribute")
  expect_error(fire_exp_extract_vis(ext_pols, method = "blah"),
               "'arg' should be one of")
  expect_error(fire_exp_extract_vis(ext_pts, classify = "blah"),
               "'arg' should be one of")
  expect_error(fire_exp_extract_vis(ext_pols, classify = "blah"),
               "'arg' should be one of")
})

test_that("fire_exp_extract_vis() returns objects with correct class", {
  exp <- exposure()
  pts <- pts(20)
  ext_pts <- fire_exp_extract(exp, pts)
  expect_s3_class(suppressWarnings(fire_exp_extract_vis(ext_pts)), "data.frame")
  expect_s3_class(suppressWarnings(fire_exp_extract_vis(ext_pts,
                                                        map = TRUE)), "ggplot")
})

test_that("fire_exp_extract_vis() runs when input conditions are met", {
  exp <- exposure()
  pts <- pts(20)
  pols <- terra::buffer(pts, 200)
  ext_pts <- fire_exp_extract(exp, pts)
  ext_pols <- fire_exp_extract(exp, pols)

  expect_no_error(suppressWarnings(fire_exp_extract_vis(ext_pts)))
  expect_no_error(suppressWarnings(fire_exp_extract_vis(ext_pts, map = TRUE)))
  expect_no_error(suppressWarnings(fire_exp_extract_vis(ext_pts,
                                                        classify = "lan")))
  expect_no_error(suppressWarnings(fire_exp_extract_vis(ext_pts,
                                                        classify = "lan",
                                                        map = TRUE)))
  expect_no_error(fire_exp_extract_vis(ext_pols, method = "mean"))
  expect_no_error(fire_exp_extract_vis(ext_pols, method = "mean", map = TRUE))
})
