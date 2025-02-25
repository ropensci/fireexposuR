test_that("fire_exp_map_cont() input checks and function messages work", {
  exp <- exposure()
  v <- pol()

  cropexp <- terra::crop(exp, terra::rescale(v, 0.5))

  nocrs <- exp
  terra::crs(nocrs) <- ""
  v2 <- v
  terra::crs(v2) <- ""

  expect_condition(fire_exp_map_cont(2),
                   "`exposure` must be a SpatRaster object")
  expect_condition(fire_exp_map_cont(exp * 2),
                   "`exposure` layer must have values between 0-1")
  expect_condition(fire_exp_map_cont(exp, 2),
                   "`aoi` must be a SpatVector object")
  expect_condition(fire_exp_map_cont(cropexp, v),
                   "`aoi` extent must be within `exposure` extent")
  expect_condition(fire_exp_map_cont(nocrs, v),
                   "`exposure` layer must have a CRS")
  expect_condition(fire_exp_map_cont(exp, v2),
                   "`exposure` and `aoi` must have same CRS")
})

test_that("fire_exp_map_cont() returns object with correct class", {
  exp <- exposure()
  expect_s3_class(suppressMessages(fire_exp_map_cont(exp)), "ggplot")
})

test_that("fire_exp_map_cont() runs when input conditions are met", {
  exp <- exposure()
  v <- pol()
  # messages suppressed because terra outputs a message for bigger rasters
  expect_no_error(suppressMessages(fire_exp_map_cont(exp)))
  expect_no_error(suppressMessages(fire_exp_map_cont(exp, v)))
})
