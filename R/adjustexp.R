#' Title
#'
#' @param hazard hazard
#' @param tdist tdist
#' @param nonburnable nonburnable
#'
#' @return SpatRaster
#' @export
#'
#' @examples
#' # coming soon
#'
adjustexp <- function(hazard, tdist, nonburnable) {
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
  message("Proceed with caution: any adjustments to transmission distances
          should be further validated with observed fire history")


  res <- terra::res(hazard)[1]
  stopifnot("insufficient resolution for chosen exposure transmission distance"
            = res <= tdist / 3)
  annulus <- c(res, tdist)
  window <- MultiscaleDTM::annulus_window(annulus, "map", res)
  wgtwindow <- window / sum(window, na.rm = TRUE)
  exp <- terra::focal(hazard, wgtwindow, fun = sum) %>%
    dplyr::rename(exposure = .data$focal_sum)
  if (missing(nonburnable)) {
    return(exp)
  } else {
    expb <- terra::mask(exp, nonburnable, inverse = TRUE)
    return(expb)
  }
}
