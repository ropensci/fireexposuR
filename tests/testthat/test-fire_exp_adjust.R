test_that("fire_exp_adjust() input checks work", {
  haz <- haz()
  expect_error(fire_exp_adjust(2),
               "`hazard` must be a SpatRaster object")
  expect_error(fire_exp_adjust(haz),
               "is missing, with no default")
  expect_error(fire_exp_adjust(haz * 2, 300),
               "`hazard` layer must have values between 0-1")
  expect_error(fire_exp_adjust(haz, "l"),
               "`tdist` must be numeric")
  expect_error(fire_exp_adjust(haz, 50),
               "insufficient resolution")
  expect_error(fire_exp_adjust(haz, 350, 4),
               "`no_burn` must be a SpatRaster object")
})

test_that("fire_exp_adjust() returns correct object class", {
  haz <- haz()
  expect_s4_class(fire_exp_adjust(haz, 350), "SpatRaster")
})

test_that("fire_exp_adjust() runs when input conditions are met", {
  haz <- haz()
  nb <- nb()
  expect_no_error(fire_exp_adjust(haz, 350))
  expect_no_error(fire_exp_adjust(haz, 350, nb))
})
