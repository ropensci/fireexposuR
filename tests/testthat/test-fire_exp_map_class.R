test_that("fire_exp_map_class() input checks and function messages work", {
  exp <- exposure()
  v <- polygon()

  cropexp <- terra::crop(exp, terra::rescale(v, 0.5))

  nocrs <- exp
  terra::crs(nocrs) <- ""
  v2 <- v
  terra::crs(v2) <- ""

  expect_error(fire_exp_map_class(2, "loc", v),
               "`exposure` must be a SpatRaster object")
  expect_error(fire_exp_map_class(exp * 2, v),
               "must have values between")
  expect_error(fire_exp_map_class(exp, 2),
               "`aoi` must be a SpatVector")
  expect_error(fire_exp_map_class(cropexp, v),
               "`aoi` extent must be within `exposure` extent")
  expect_error(fire_exp_map_class(exp, v, "blah"),
               "'arg' should be one of")
  expect_error(fire_exp_map_class(nocrs, v, "lan"),
               "`exposure` layer must have a CRS defined")
  expect_error(fire_exp_map_class(exp, v2, "lan"),
               "`exposure` and `aoi` must have same CRS")
})

test_that("fire_exp_map_class() returns object with correct class", {
  exp <- exposure()
  v <- polygon()
  expect_s3_class(suppressWarnings(fire_exp_map_class(exp, aoi = v)), "ggplot")
})

test_that("fire_exp_map_class() runs when input conditions are met", {
  exp <- exposure()
  v <- polygon()
  expect_no_error(suppressWarnings(fire_exp_map_class(exp, v, "lan")))
  expect_no_error(fire_exp_map_class(exp, v, "cus", class_breaks = c(0.2, 1)))
})
