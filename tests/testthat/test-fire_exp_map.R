test_that("fire_exp_map() input checks and function messages work", {
  exp <- exposure()
  v <- polygon()

  cropexp <- terra::crop(exp, terra::rescale(v, 0.5))

  nocrs <- exp
  terra::crs(nocrs) <- ""
  v2 <- v
  terra::crs(v2) <- ""

  expect_error(fire_exp_map(2, "loc", v),
               "`exposure` must be a SpatRaster object")
  expect_error(fire_exp_map(exp * 2, v),
               "must have values between")
  expect_error(fire_exp_map(exp, 2),
               "`aoi` must be a SpatVector")
  expect_error(fire_exp_map(cropexp, v),
               "`aoi` extent must be within `exposure` extent")
  expect_error(fire_exp_map(exp, v, classify = "blah"),
               "`classify` must be one of:")
  expect_error(fire_exp_map(nocrs, v),
               "`exposure` layer must have a CRS defined")
  expect_error(fire_exp_map(exp, v2),
               "`exposure` and `aoi` must have same CRS")
  expect_error(fire_exp_map(exp, v, "custom", c("a", "b")),
               "`class_breaks` must be a vector of numbers")
  expect_error(fire_exp_map(exp, v, "custom", c(0.2, 0.8)),
               "`class_breaks` must have 1 as the maximum value")
  expect_error(fire_exp_map(exp, v, "custom", c(-0.2, 1)),
               "`class_breaks` must be greater than 0")
})

test_that("fire_exp_map() returns object with correct class", {
  exp <- exposure()
  v <- polygon()
  expect_s3_class(suppressMessages(fire_exp_map(exp, aoi = v)), "tmap")
})

test_that("fire_exp_map() runs when input conditions are met", {
  exp <- exposure()
  v <- polygon()
  expect_no_error(suppressMessages(fire_exp_map(exp, v, "landscape")))
  expect_no_error(fire_exp_map(exp, v, classify = "custom",
                               class_breaks = c(0.2, 1)))
})
