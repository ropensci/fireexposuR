test_that("fire_exp_dir_map() input checks work", {
  exp <- exposure()
  v <- pol()
  pt <- pts(1)

  t_pt <- fire_exp_dir(exp, pt)
  t_pol <- fire_exp_dir(exp, v)
  expect_error(fire_exp_dir_map(2),
               "`transects` must be a SpatVector object")
  expect_error(fire_exp_dir_map(t_pt, title = 2),
               "`title` must be")
  expect_error(fire_exp_dir_map(t_pt, labels = "blah"),
               "`labels` must be") # not enough
  expect_error(fire_exp_dir_map(t_pt, labels = c("blah", "blah")),
               "`labels` must be") # not characters
  expect_error(suppressMessages(fire_exp_dir_map(t_pt, value = 2)),
               "`value` must be a SpatVector object")
})

test_that("fire_exp_dir_map() returns objects with correct class", {
  exp <- exposure()
  v <- pol()
  pt <- pts(1)

  t_pt <- fire_exp_dir(exp, pt)
  t_pol <- fire_exp_dir(exp, v)
  expect_s3_class(suppressMessages(fire_exp_dir_map(t_pt)), "ggplot")
})

test_that("fire_exp_dir_map() runs when input conditions are met", {
  exp <- exposure()
  v <- pol()
  pt <- pts(1)

  t_pt <- fire_exp_dir(exp, pt)
  t_pol <- fire_exp_dir(exp, v)
  expect_no_error(suppressMessages(fire_exp_dir_map(t_pt)))
  expect_no_error(suppressMessages(fire_exp_dir_map(t_pol)))
  expect_no_error(suppressMessages(fire_exp_dir_map(t_pol, value = v)))
  expect_no_error(suppressMessages(fire_exp_dir_map(t_pt,
                                                    labels = c("blah",
                                                               "blah",
                                                               "blah"))))
  expect_no_error(suppressMessages(fire_exp_dir_map(t_pt,
                                                    title = "blah blah blah")))
})
