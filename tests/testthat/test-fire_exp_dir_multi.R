test_that("fire_exp_dir_multi() input checks and function messages work", {
  exp <- exposure()
  pts <- pts(n = 3) # reduced for speed
  expect_error(fire_exp_dir_multi(2, pts),
               "`exposure` must be a SpatRaster object")
  expect_error(fire_exp_dir_multi(exp, 2),
               "`values` must be a SpatVector")
})

test_that("fire_exp_dir_multi() runs when input conditions are met", {
  exp <- exposure()
  pts <- pts(n = 3) # reduced for speed
  expect_no_error(fire_exp_dir_multi(exp, pts, plot = TRUE, interval = 10))
  expect_no_error(fire_exp_dir_multi(exp, pts, interval = 10))
})
