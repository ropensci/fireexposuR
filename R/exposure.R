#' Calculate Exposure
#'
#' @param hazard a SpatRaster that represents hazardous fuels for the
#' transmission distance specified in tdist
#' @param tdist a character vector, can be 'l' for long-range embers, 's' for
#'     short-range embers, or 'r' for radiant heat
#' @param nonburnable (optional) a spatRaster that represents the burnable
#'     landscape. Any cells that cannot receive wildfire (e.g. open water,
#'     rocks) should be of value 1, all other cells should be NODATA
#'
#' @return a SpatRaster
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' lhaz <- terra::rast(system.file("extdata/LHazAB2020.tif", package = "fireexposuR"))
#' nonburnable <- terra::rast(system.file("extdata/nonburnableAB2020.tif", package = "fireexposuR"))
#' lexp <- exposure(lhaz, tdist = "l")
#' lexpburnable <- exposure(lhaz, tdist = "l", nonburnable)
#'
exposure <- function(hazard, tdist = c("l", "s", "r"), nonburnable) {
  haz = hazard
  res <- terra::res(haz)[1]
  if (tdist == "l") {
    if (res > 150) {
      stop("Error: insufficient data resolution for
           Long-Range Exposure Assessment")
    } else {
      annulus <- c(res, 500)
      window <- MultiscaleDTM::annulus_window(annulus, "map", res)
    }
  }
  if (tdist == "s") {
    if (res > 33) {
      stop("Error: insufficient data resolution for
           Short-Range Exposure Assessment")
    } else {
      #create moving window of 100 m
      annulus <- c(res, 100)
      window <- MultiscaleDTM::annulus_window(annulus, "map", res)
    }
  }
  if (tdist == "r") {
    if (res > 10) {
      stop("Error: insufficient data resolution for
           Radiant Exposure Assessment")
    } else {
      #create moving window of 30 m
      annulus <- c(res, 30)
      window <- MultiscaleDTM::annulus_window(annulus, "map", res)
    }
  }
  wgtwindow <- window / sum(window, na.rm = TRUE)
  exp <- terra::focal(haz, wgtwindow, fun = sum) %>%
    tidyterra::rename(exposure = .data$focal_sum)
  if (missing(nonburnable)) {
    return(exp)
  } else {
    expb <- terra::mask(exp, nonburnable, inverse = TRUE)
    return(expb)
  }
}
