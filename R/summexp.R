#' Summarize Exposure
#'
#' @param exp SpatRaster from exposure()
#' @param aoi SpatVector to mask exposure to for summary
#' @param classify character, either "landscape" or "local"
#'
#' @return data frame
#' @export
#'
#' @examples
#'
#' lexp <- terra::rast(system.file("extdata/LExpAB2020.tif", package = "fireexposuR"))
#' fpa <- terra::vect(system.file("extdata/fpa.shp", package = "fireexposuR"))
#' WHITpoly <- terra::vect(system.file("extdata/WHITpoly.shp", package = "fireexposuR"))
#'
#' #summary table for landscape classification, entire extent
#' summexp(lexp, classify = "landscape")
#'
#' #summary table for landscape classification, for forest protection area
#' summexp(lexp, fpa, classify = "landscape")
#'
#' # summary table for local classification, built enviro of a community
#' summexp(lexp, WHITpoly, classify = "local")
#'
summexp <- function(exp, aoi, classify = c("landscape", "local")) {
  if (terra::linearUnits(exp) != 1) {
    stop("Linear units of exposure layer must be in meters")
  }
  if (missing(classify)) {
    stop("please indicate either local or landscape for classify parameter")
  }
  res <- terra::res(exp)[1]
  lut <- 0:5
  names(lut) <- c("Nil", "Low", "Moderate", "High", "Very High", "Extreme")

  if (!missing(aoi)) {
    aoi <- terra::project(aoi, exp)

    exp <- exp %>%
      terra::crop(aoi) %>%
      terra::mask(aoi)
  }
  df <- as.data.frame(exp)
  if (classify == "landscape") {
    df <- df %>%
      dplyr::mutate(classexp = as.factor(
        dplyr::case_when(
          exposure >= 0.8 ~ 5,
          exposure >= 0.6 ~ 4,
          exposure >= 0.4 ~ 3,
          exposure >= 0.2 ~ 2,
          exposure >= 0 ~ 1
        )
      ))
  }
  if (classify == "local") {
    df <- df %>%
      dplyr::mutate(classexp = as.factor(
        dplyr::case_when(
          exposure >= 0.45 ~ 5,
          exposure >= 0.3 ~ 3,
          exposure >= 0.15 ~ 2,
          exposure > 0 ~ 1,
          exposure == 0 ~ 0
        )
      ))
  }


  df <- df %>%
    dplyr::count(.data$classexp) %>%
    dplyr::mutate(class = names(lut)[match(.data$classexp, lut)]) %>%
    dplyr::mutate(npixels = .data$n) %>%
    dplyr::mutate(prop = .data$npixels / sum(.data$npixels)) %>%
    dplyr::mutate(aream2 = .data$npixels * res * res) %>%
    dplyr::mutate(areaha = .data$aream2 / 10000) %>%
    dplyr::select(-c(.data$classexp, .data$n))

  return(df)
}
