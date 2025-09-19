#' Summarize directional load for multiple values
#'
#' @description `fire_exp_dir_multi()` summarizes the directional vulnerability
#' load for multiple points in a study area in a table.
#'
#' @details
#' This function summarizes multiple directional vulnerability assessments into
#' a single table. For each degree, the frequency of input values with
#' a continuous pathway at that trajectory is found. This summary can be useful
#' in identifying trends in directional exposure to values within a regional
#' area of interest.
#'
#' Continuous pathways are assessed for the full span of all three
#' directional assessment transect segments, or limited to the outer two
#' segments. If the values being assessed are variable sizes and being
#' represented as points, using the outer option is recommended. The inner-most
#' segment is sensitive to the size of the value when a point is used.
#' Adjusting the parameters for `fire_exp_dir()` is also supported. See details
#' in [`fire_exp_dir()`] for more information.
#'
#' @references
#' Beverly JL, Forbes AM (2023) Assessing directional vulnerability to
#' wildfire. *Natural Hazards* **117**, 831-849.
#' \doi{10.1007/s11069-023-05885-3}
#'
#'
#' @param exposure SpatRaster from [`fire_exp()`]
#' @param values Spatvector of value as a point or simplified polygon
#' @param ... arguments passed to [`fire_exp_dir()`].
#'
#' @return a data.frame of the features with attributes: value featureID,
#' degree, seg1 (binary), seg2 (binary), seg3 (binary), full (binary),
#' outer (binary).
#' @export
#'
#' @examples
#' # read example hazard data
#' hazard_file_path <- "extdata/hazard.tif"
#' hazard <- terra::rast(system.file(hazard_file_path, package = "fireexposuR"))
#'
#' # generate 10 random example points within the hazard extent
#' e <- terra::buffer(terra::vect(terra::ext(hazard), crs = hazard), -15500)
#' points <- terra::spatSample(e, 10)
#'
#' # compute exposure metric
#' exposure <- fire_exp(hazard)
#'
#' # directional load for multiple points
#' fire_exp_dir_multi(exposure, points, interval = 10)

fire_exp_dir_multi <- function(exposure, values, ...) {
  stopifnot("`exposure` must be a SpatRaster object"
            = class(exposure) == "SpatRaster",
            "`exposure` layer must have values between 0-1"
            = (round(terra::minmax(exposure)[1], 0) >= 0
               && round(terra::minmax(exposure)[2], 0) <= 1),
            "`values` must be a SpatVector object of point or polygon features"
            = (class(values) == "SpatVector" &&
                 terra::geomtype(values) %in% c("points", "polygons")),
            "`values` and `exposure` must have the same crs"
            = terra::same.crs(values, exposure) == TRUE)

  names(exposure) <- "exposure"
  expl <- exposure
  fts <- values

  df <- data.frame()

  for (i in seq_len(nrow(fts))) {
    dat <- fire_exp_dir(expl, fts[i], table = TRUE, ...) %>%
      dplyr::select(-"wkt") %>%
      dplyr::mutate(featureID = i) %>%
      tidyr::pivot_wider(names_from = "seg", values_from = "viable") %>%
      dplyr::select("featureID", tidyselect::everything())
    df <- rbind(df, dat)
  }

  df2 <- df %>%
    dplyr::mutate(full = ifelse(.data$seg1 + .data$seg2 + .data$seg3 == 3,
                                1, 0)) %>%
    dplyr::mutate(outer = ifelse(.data$seg2 + .data$seg3 == 2, 1, 0))

  return(df2)
}
