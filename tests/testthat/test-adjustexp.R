# generate example hazard data -----------------------------
set.seed(0)
e <- c(45,55,495,505) * 10000
r <- terra::rast(resolution = 100, extent = terra::ext(e))
terra::values(r) <- sample(c(0,1), terra::ncell(r), replace = TRUE)
r <- terra::sieve(r, threshold = 50, directions = 4)
haz <- terra::sieve(r, threshold = 500, directions = 4)

hazvals <- haz *10

test_that("adjustexp() input checks work", {
  expect_error(adjustexp(2))
  expect_error(adjustexp(hazbig))
  expect_error(adjustexp(haz, "h"))
  expect_error(adjustexp(haz, 50))
})

test_that("adjustexp() returns correct object class", {
  expect_s4_class(adjustexp(haz, 350), "SpatRaster")
})

test_that("adjustexp() runs when input conditions are met", {
  expect_no_error(adjustexp(haz, 350))
  expect_message(adjustexp(haz, 350))
})
