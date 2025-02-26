test_that("fire_exp_dir_plot() input checks work", {
  exp <- exposure()
  v <- pol()
  pt <- pts(1)

  t_pt <- fire_exp_dir(exp, pt)
  t_pol <- fire_exp_dir(exp, v)
  expect_error(fire_exp_dir_plot(2),
               "`transects` must be a SpatVector object")
  expect_error(fire_exp_dir_plot(t_pt, title = 2),
               "`title` must be")
  expect_error(fire_exp_dir_plot(t_pt, labels = "blah"),
               "`labels` must be") # not enough
  expect_error(fire_exp_dir_plot(t_pt, labels = c("blah", "blah")),
               "`labels` must be") # not characters
})

test_that("fire_exp_dir_plot() returns objects with correct class", {
  exp <- exposure()
  pt <- pts(1)

  t_pt <- fire_exp_dir(exp, pt)
  expect_s3_class(fire_exp_dir_plot(t_pt), "ggplot")
})

test_that("fire_exp_dir_plot() runs when input conditions are met", {
  exp <- exposure()
  v <- pol()
  pt <- pts(1)

  t_pt <- fire_exp_dir(exp, pt)
  t_pol <- fire_exp_dir(exp, v)
  expect_no_error(fire_exp_dir_plot(t_pt))
  expect_no_error(fire_exp_dir_plot(t_pol))
  expect_no_error(fire_exp_dir_plot(t_pt, labels = c("blah", "blah", "blah")))
  expect_no_error(fire_exp_dir_plot(t_pt, title = "blah blah blah"))
})
