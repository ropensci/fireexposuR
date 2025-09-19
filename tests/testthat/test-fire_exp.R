test_that("fire_exp() input checks and function messages work", {
  haz <- haz()
  nb <- nb()
  v <- polygon()
  smallhaz <- terra::crop(haz, terra::rescale(v, 0.5), mask = TRUE)
  nocrsh <- haz
  terra::crs(nocrsh) <- ""
  nocrsnb <- nb
  terra::crs(nocrsnb) <- ""
  expect_error(fire_exp(5),
               "`hazard` must be a SpatRaster object")
  expect_error(fire_exp(haz * 2),
               "`hazard` layer must have values between 0-1")
  expect_warning(fire_exp(haz, tdist = "l"),
                 "use of the 'tdist' parameter has been deprecated.")
  expect_error(fire_exp(haz, t_dist = c("l", 3)),
               "only one value")
  expect_error(fire_exp(haz, t_dist = nb),
               "`t_dist` must be numeric")
  expect_message(fire_exp(nocrsh),
                 "Input CRS is undefined:")
  expect_error(fire_exp(terra::rescale(haz, 2)),
               "insufficient resolution for ")
  expect_error(fire_exp(haz, t_dist = 100),
               "insufficient resolution for")
  expect_error(fire_exp(haz, t_dist = 30),
               "insufficient resolution for ")
  expect_error(fire_exp(smallhaz),
               "Extent of hazard raster too small for exposure assessment")
  expect_error(fire_exp(haz, no_burn = 5),
               "`no_burn` must be a SpatRaster")
  expect_error(fire_exp(haz, no_burn = nocrsnb),
               "no_burn` and `hazard` must have same CRS")
  expect_error(fire_exp(haz, no_burn = nb * 2),
               "must only contain values of 1 or NA")
  expect_error(fire_exp(haz, no_burn = terra::extend(nb, 50, fill = 1)),
               "extent must be within `hazard` extent")
})

test_that("fire_exp() returns object with correct class", {
  haz <- haz()
  nb <- nb()
  expect_s4_class(fire_exp(haz), "SpatRaster")
  expect_s4_class(fire_exp(haz, no_burn = nb), "SpatRaster")
})

test_that("fire_exp() runs when input conditions are met", {
  haz <- haz()
  nb <- nb()
  expect_no_condition(fire_exp(haz))
  expect_no_condition(fire_exp(haz, t_dist = 800))
  expect_no_condition(fire_exp(haz * 0.5))
  expect_no_condition(fire_exp(haz, no_burn = nb))
})
