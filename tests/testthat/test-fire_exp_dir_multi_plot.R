test_that("fire_exp_dir_multi_plot() input checks and function messages work", {
  expect_error(fire_exp_dir_multi_plot(2),
               "`dir_multi` missing required attributes.")
})

test_that("fire_exp_dir_multi_plot() returns object with correct class", {
  exp <- exposure()
  pts <- pts(n = 3) # reduced for speed
  dir_multi <- fire_exp_dir_multi(exp, pts)
  expect_s3_class(fire_exp_dir_multi_plot(dir_multi), "ggplot")
})

test_that("fire_exp_dir_multi_plot() runs when input conditions are met", {
  exp <- exposure()
  pts <- pts(n = 3) # reduced for speed
  dir_multi <- fire_exp_dir_multi(exp, pts)
  expect_no_error(fire_exp_dir_multi_plot(dir_multi))
})
