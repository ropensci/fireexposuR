exposure <- function(haz, tdist = c("l", "s", "r"), burnable) {
  res <- terra::res(haz)[1]
  if (tdist == "l") {
    if (res > 150) {
      stop("Error: insufficient data resolution for Long-Range Exposure Assessment")
    } else {
    annulus <- c(res, 500)
    window <- MultiscaleDTM::annulus_window(annulus, "map", res)
    }
  }
  if (tdist == "s") {
    if (res > 33) {
      stop("Error: insufficient data resolution for Short-Range Exposure Assessment")
    } else {
      #create moving window of 100 m
      annulus <- c(res, 100)
      window <- MultiscaleDTM::annulus_window(annulus, "map", res)
    }
  }
  if (tdist == "r"){
    if (res > 10) {
      stop("Error: insufficient data resolution for Radiant Exposure Assessment")
    } else {
      #create moving window of 30 m
      annulus <- c(res, 30)
      window <- MultiscaleDTM::annulus_window(annulus, "map", res)
    }
  }
  wgtwindow <- window / sum(window, na.rm = T)
  exp <- terra::focal(haz, wgtwindow, fun = sum) %>%
    rename(exposure = focal_sum)
  if (missing(burnable)){
    return(exp)
  } else {
  expb <- terra::mask(exp,burnable)
  return(expb)
  }
}
