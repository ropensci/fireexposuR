test_that("fire_exp_dir() input checks and function messages work", {
  pt <- pts(1)
  pts <- pts(2)
  exp <- exposure()
  expect_error(fire_exp_dir(2, pt),
               "`exposure` must be a SpatRaster object")
  expect_error(fire_exp_dir(exp, 2),
               "`value` must be a SpatVector object")
  expect_message(fire_exp_dir(exp, pts),
                 "Value object provided has more than one feature")
  expect_error(fire_exp_dir(exp, pt, t_lengths = c("a", 2, 3)),
               "`t_lengths` must be a vector of three numeric values")
  expect_error(fire_exp_dir(exp, pt, t_lengths = c(1, 2)),
               "`t_lengths` must be a vector of three numeric values")
  expect_error(fire_exp_dir(exp, pt, interval = "a"),
               "`interval` must be one of: ")
  expect_error(fire_exp_dir(exp, pt, thresh_exp = "a"),
               "`thresh_exp` must be a numeric value between 0-1")
  expect_error(fire_exp_dir(exp, pt, thresh_viable = 2),
               "`thresh_viable` must be a numeric value between 0-1")
})


test_that("fire_exp_dir() returns object with correct class", {
  pt <- pts(1)
  exp <- exposure()
  expect_s4_class(fire_exp_dir(exp, pt), "SpatVector")
  expect_s3_class(fire_exp_dir(exp, pt, table = TRUE), "data.frame")
})

test_that("fire_exp_dir() runs when input conditions are met", {
  pt <- pts(1)
  v <- pol()
  exp <- exposure()
  expect_no_error(fire_exp_dir(exp, pt))
  expect_no_error(fire_exp_dir(exp, v))
  expect_no_error(fire_exp_dir(exp, pt, table = TRUE))
  expect_no_error(fire_exp_dir(exp, v, table = TRUE))
  expect_no_error(fire_exp_dir(exp, v, t_lengths = c(2000, 2000, 2000)))
  expect_no_error(fire_exp_dir(exp, v, interval = 5))
  expect_no_error(fire_exp_dir(exp, pt, thresh_exp = 0.5))
  expect_no_error(fire_exp_dir(exp, pt, thresh_viable = 0.5))
})
