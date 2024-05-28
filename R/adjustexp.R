adjustexp <- function(hazard, tdist, nonburnable) {
  haz <- hazard
  warning("Proceed with caution: any adjustments to transmission distances
          should be further validated with observed fires")
  res <- terra::res(haz)[1]
  if (res < tdist / 3) {
    stop("insufficient data resolution for chosen exposure transmission
         distance")
  }  else {
    annulus <- c(res, tdist)
    window <- MultiscaleDTM::annulus_window(annulus, "map", res)
  }
  exp <- terra::focal(haz, window, fun = sum) %>%
    dplyr::rename(exposure = .data$focal_sum)
  if (missing(nonburnable)) {
    return(exp)
  } else {
    expb <- terra::mask(exp, nonburnable)
    return(expb)
  }
}
