# generate test data ===========================================================
set.seed(0)
e <- c(45,55,495,505) * 10000
r <- terra::rast(resolution = 100, extent = terra::ext(e))
terra::values(r) <- sample(c(0,1), terra::ncell(r), replace = TRUE)
r <- terra::sieve(r, threshold = 50, directions = 4)
# input: hazard with no CRS (100 m resolution)
haz <- terra::sieve(r, threshold = 500, directions = 4)
# input: hazard with CRS
hazcrs <- haz
terra::crs(hazcrs) <- "EPSG:32608"
# input: hazard without values from 0-1
hazvals <- haz * 10
# input: hazard with too coarse of a resolution
haz200m <- terra::rescale(hazcrs, 2)
# input: hazard with too small of an extent
e2 <- c(4500,4503,49500,49503) * 100
r2 <- terra::rast(resolution = 100, extent = terra::ext(e2), crs = "EPSG:32608")
terra::values(r2) <- sample(c(0,1), terra::ncell(r2), replace = TRUE)
# input : hazard with proportional hazard vals
hazalt <- terra::rast(resolution = 100, extent = terra::ext(e), crs = "EPSG:32608")
suppressWarnings(terra::values(hazalt) <- (0:5)/5)
# input : nonburnable, with no CRS
nb <- terra::rast(resolution = 100, extent = terra::ext(e))
terra::values(nb) <- suppressWarnings(sample(c(NA, 1), terra::ncell(nb),
                            replace = TRUE, prob = c(0.9,0.1)))
# input: nonburnable, with CRS
nbcrs <- nb
terra::crs(nbcrs) <- "EPSG:32608"
# input: nonburnable, wrong values
nbvals <- nbcrs * 2
# input: nonburnable, big
nbbig <- terra::extend(nbcrs, 50, fill = 1)

# tests ========================================================================

test_that("exposure() input checks and function messages work", {
  expect_condition(exposure(5)) # stopifnot() L50
  expect_condition(exposure(haz * 2)) # stopifnot() L52
  expect_condition(exposure(hazcrs, tdist = "x")) # matcharg() L54
  expect_condition(exposure(haz)) # message() L57
  expect_condition(exposure(terra::rescale(hazcrs, 2))) # stopifnot() L65
  expect_condition(exposure(hazcrs, tdist = "s")) # stopifnot() L71
  expect_condition(exposure(hazcrs, tdist = "r")) # stopifnot() L77
  expect_condition(exposure(r2)) # stopifnot() L82
  expect_condition(exposure(hazcrs, nonburnable = 5)) # stopifnot() L90
  expect_condition(exposure(hazcrs, nonburnable = nb)) # stopifnot() L92
  expect_condition(exposure(hazcrs, nonburnable = nbcrs *2)) # stopifnot() L94
  expect_condition(exposure(hazcrs, nonburnable = nbbig)) # stopifnot() L96
})

test_that("exposure() returns object with correct class", {
  expect_s4_class(exposure(hazcrs), "SpatRaster")
  expect_s4_class(exposure(hazcrs, nonburnable = nbcrs), "SpatRaster")
})

test_that("exposure() runs when input conditions are met", {
  expect_no_condition(exposure(hazcrs))
  expect_no_condition(exposure(hazalt))
  expect_no_condition(exposure(hazcrs, nonburnable = nbcrs))
})
