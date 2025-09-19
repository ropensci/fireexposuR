test_that("fire_exp_validate() input checks and function messages work", {
  expnb <- exposure(nb())
  fires <- fires()
  expect_error(fire_exp_validate(2, fires),
               "`burnableexposure` must be a SpatRaster object")
  expect_error(fire_exp_validate(expnb, 2),
               "`fires` must be a SpatVector object")
  expect_error(fire_exp_validate(expnb, fires, 2),
               "`aoi` must be a SpatVector object")
})

test_that("valdiateexp() returns object with correct class", {
  expnb <- exposure(nb())
  fires <- fires()
  expect_s3_class(fire_exp_validate(expnb, fires), "data.frame")
})

test_that("fire_exp_validate() runs when input conditions are met", {
  expnb <- exposure(nb())
  fires <- fires()
  expect_no_error(fire_exp_validate(expnb, fires))
  expect_no_error(fire_exp_validate(expnb, fires, samplesize = 0.2))
})

test_that("fire_exp_validate() randomly samples", {
  expnb <- exposure(nb())
  fires <- fires()
  set.seed(0)
  output1 <- fire_exp_validate(expnb, fires)
  set.seed(1)
  output2 <- fire_exp_validate(expnb, fires)
  set.seed(0)
  output3 <- fire_exp_validate(expnb, fires)
  expect_false(identical(output1, output2))
  expect_true(identical(output1, output3))
})
