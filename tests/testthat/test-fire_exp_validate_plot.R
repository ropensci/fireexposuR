test_that("fire_exp_valdiate_plot() returns object with correct class", {
  expnb <- exposure(nb())
  fires <- fires()
  set.seed(0)
  output1 <- fire_exp_validate(expnb, fires)
  expect_s3_class(fire_exp_validate_plot(output1), "ggplot")
})
