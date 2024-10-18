#' Compute the wildfire exposure metric with custom transmission distances
#'
#' @description For advanced users. If the transmission distances from the
#' wildfire exposure literature are not representative of the wildland fuels
#' in your area of interest, this function can be used to change the
#' transmission distance to a custom distance. It is highly recommended that
#' any exposure layers produced with this function are validated with observed
#' fire history using the [fire_exp_validate()] function.
#'
#'
#' @param hazard a SpatRaster that represents hazardous fuels for the
#'   transmission distance specified in `tdist`
#' @param tdist Numeric, transmission distance in meters
#' @param nonburnable (Optional)  SpatRaster that represents the non-burnable
#'   landscape. Any cells that cannot receive wildfire (e.g. open water, rock)
#'   and any cells that are not natural (e.g. built environment,
#'   irrigated agricultural areas) should be of value 1, all other cells
#'   should be NODATA. This parameter should be provided if preparing data
#'   for [fire_exp_validate()]
#'
#' @return SpatRaster object of exposure values
#' @export
#'
#' @examples
#' # read example hazard data ----------------------------------
#' filepath <- "extdata/hazard.tif"
#' haz <- terra::rast(system.file(filepath, package = "fireexposuR"))
#' # -----------------------------------------------------------
#'
#' # compute long range exposure with custom disance of 800 m
#' exp <- fire_exp_adjust(haz, tdist = 800)
#'
fire_exp_adjust <- function(hazard, tdist, nonburnable) {
  stopifnot("`hazard` must be a SpatRaster object"
            = class(hazard) == "SpatRaster")
  stopifnot("`hazard` layer must have values between 0-1"
            = (terra::minmax(hazard)[1] >= 0 && terra::minmax(hazard)[2] <= 1))
  stopifnot("`tdist` must be numeric"
            = is.numeric(tdist))
  if (!missing(nonburnable)) {
    stopifnot("`nonburnable` must be a SpatRaster object"
              = class(nonburnable) == "SpatRaster")
  }

  res <- terra::res(hazard)[1]
  stopifnot("insufficient resolution for chosen exposure transmission distance"
            = res <= tdist / 3)
  annulus <- c(res, tdist)
  window <- MultiscaleDTM::annulus_window(annulus, "map", res)
  wgtwindow <- window / sum(window, na.rm = TRUE)
  exp <- terra::focal(hazard, wgtwindow, fun = sum) %>%
    tidyterra::rename(exposure = "focal_sum")
  if (missing(nonburnable)) {
    return(exp)
  } else {
    expb <- terra::mask(exp, nonburnable, inverse = TRUE)
    return(expb)
  }
}
