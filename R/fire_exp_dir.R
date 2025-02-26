#' Conduct a directional exposure assessment
#'
#' @description `fire_exp_dir()` returns a SpatVector of linear transects toward
#' a value. Transects are assessed as viable wildfire pathways by intersecting
#' them with areas of high exposure.
#'
#'
#' @details
#' `fire_exp_dir()` automates the directional vulnerability assessment
#' methods documented in Beverly and Forbes (2023). This analysis is used to
#' assess linear wildfire vulnerability on the landscape in a systematic radial
#' sampling pattern. This is a landscape scale process, so the exposure raster
#' used should also be landscape scale. Use `tdist = "l"` when preparing data
#' with [`fire_exp()`] for use with this function. See [`fire_exp()`] details
#' for more information.
#'
#' The output line features will have the attribute 'viable'
#' which can be used to visualize the pathways. Outputs can be visualized with
#' [`fire_exp_dir_plot()`], [`fire_exp_dir_map()`], or exported as a spatial
#' feature.
#'
#' ## Spatial Reference
#'
#' The inputs for the exposure and value layer must have the same coordinate
#' reference system (CRS) defined. The transects will be returned in the same
#' CRS as the inputs.
#'
#' This function draws the transects by calculating the end point of a
#' transect by finding the shortest path along the WGS84
#' ([EPSG:4326](https://epsg.io/5326)) ellipsoid at a given bearing and
#' distance. The `value` input is reprojected from the input CRS to latitude and
#' longitude using WGS84 for the calculations. After the transects are created,
#' they are projected to match the CRS of the input exposure and value layer.
#' The lengths of the projected transects will be effected by the scale factor
#' of the input CRS; however, the geodesic lengths are maintained.
#'
#' ## Value feature
#'
#' The value feature can be provided as a point or a simplified polygon.
#'
#' If using a point feature the analysis can be sensitive to the placement
#' of a point. For example, if using a point to represent a large town a
#' point placed at the centroid will likely have different outputs than a
#' point placed at the edge of the community due to the arrangement of lower
#' exposure values typical of a built environment.
#'
#' An option to use a simplified polygon has been added to this function for
#' values that may be too large to represent with a single point. The polygon
#' should be drawn with the consideration of the following or the function
#' will not be able to run. The polygon must be a single part polygon with no
#' holes. The polygon should have a smooth and simple shape, ideally circular
#' or ellipsoidal. The polygon should also have an approximate radius of less
#' than 5000 meters from the center. If the area of interest is larger than
#' this then consider using multiple assessments.
#'
#' ## Default Values
#' The default values are based on the methods used and validated in Alberta,
#' Canada by Beverly and Forbes (2023). Options have been added to the
#' function to allow these values to be adjusted by the user if required.
#' Adjusting these values can lead to unexpected and uncertain results.
#'
#' ## Adjusting the transects
#' The drawing of the transects can be customized by varying the intervals and
#' segment lengths if desired. Adjustments to the `interval` and `t_lengths`
#' parameters will effect how much of the exposure data is being captured by
#' the transects. If both parameters are being adjusted some trigonometry might
#' be required to find the optimal combination of the parameters to ensure
#' distances between the transects make sense for the intended application. The
#' resolution of the exposure raster may also be a factor in these decisions.
#'
#' ### Interval
#' The interval parameter defines how many transects are drawn. The default of
#' `1` draws a transect at every whole degree from 1-360. This outputs a total
#' of 1080 transects, 360 for each segment. Increasing the interval will
#' output less transects and process faster. When the interval is
#' increased, the distance between the ends of the transects will also be
#' increased. For example: the terminus of 15000 meter transects (the default)
#' drawn every 1 degree (the default) will be approximately 250 meters apart,
#' but if drawn at 10 degree intervals will be approximately 2500 meters
#' apart. Larger intervals will increase speed and processing time, but might
#' not capture potential pathways between transects farther from the value.
#'
#' ### Lengths
#' The t_lengths parameter allows a custom distance to be defined for the three
#' transect segments in meters. Lengths can be increased or decreased. The
#' segments can also be different lengths if desired.
#'
#' ## Adjusting the thresholds
#' Threshold adjustments should only be considered if validation within the
#' area of interest have been done. The function [fire_exp_validate()] has been
#' included with this package to make the process easier, but still requires
#' significant time, data, and understanding.
#'
#' ### High exposure
#' The `thresh_exp` parameter can be adjusted to define the minimum exposure
#' value to be considered for the directional assessment. The default value of
#' `0.6` is based on the findings of Beverly et al. (2021) who showed that
#' observed fires burned preferentially in areas where wildfire exposure values
#' exceed 60%. Adjusting this value is only recommended after a validation of
#' wildfire exposure has been conducted in the area of interest.
#'
#' ### Viability
#' The `thresh_viable` parameter defines the minimum intersection with high
#' exposure areas to be considered a viable pathway. The default value of
#' `0.8` was determined by Beverly and Forbes (2023) by drawing continuous
#' linear transects within burned areas to represent observed pathways. It was
#' found that the average intersection with patches of pre-fire high exposure
#' was 80%. This methodology could be repeated in the users area of interest.
#'
#' @references
#' Beverly JL, Forbes AM (2023) Assessing directional vulnerability to
#' wildfire. *Natural Hazards* **117**, 831-849.
#' [DOI](https://doi.org/10.1007/s11069-023-05885-3)
#'
#' Beverly JL, McLoughlin N, Chapman E (2021) A simple metric of landscape
#' fire exposure. *Landscape Ecology* **36**, 785-801.
#' [DOI](https://doi.org/10.1007/s10980-020-01173-8)
#'
#'
#' @param exposure SpatRaster (e.g. from [fire_exp()])
#' @param value Spatvector of value as a point or simplified polygon
#' @param t_lengths (Optional) A vector of three numeric values. The length of
#'  each transect starting from the value in meters. The default is
#'  `c(5000, 5000, 5000)`.
#' @param interval (Optional) Numeric. The degree interval at which to draw
#'  the transects for analysis. Can be one of 0.25, 0.5, 1, 2, 3, 4, 5, 6, 8,
#'  or 10 (factors of 360, ensures even spacing). The default is `1`.
#' @param thresh_exp (optional) Numeric. The exposure value to use to define
#'  high exposure as a proportion. Must be between 0-1. The default is `0.6`.
#' @param thresh_viable (optional) Numeric. The minimum intersection of a
#'  transect with a high exposure patch to be defined as a viable pathway as
#'  a proportion. Must be between 0-1. The default is `0.8`.
#' @param table Boolean, when `TRUE`: returns a table instead of a feature. The
#'  default is `FALSE`.
#'
#' @return a SpatVector of the transects with the attributes: 'deg' = degree,
#' 'seg' = segment, and 'viable'. The transects will be returned with the same
#' CRS as the input features.
#'
#' If `table = TRUE`: a data frame is returned instead with an additional
#' attribute 'wkt', which is a Well Known Text (WKT) string of transect
#' geometries (coordinate reference system: WGS84).
#'
#' @export
#'
#' @examples
#' # read example hazard data
#' hazard_file_path <- "extdata/hazard.tif"
#' hazard <- terra::rast(system.file(hazard_file_path, package = "fireexposuR"))
#'
#' # generate an example point
#' point_wkt <- "POINT (400000 6050000)"
#' point <- terra::vect(point_wkt, crs = hazard)
#'
#' # compute exposure metric
#' exposure <- fire_exp(hazard)
#'
#' # assess directional exposure
#' fire_exp_dir(exposure, point)
#'



fire_exp_dir <- function(exposure, value,
                         t_lengths = c(5000, 5000, 5000),
                         interval = 1,
                         thresh_exp = 0.6,
                         thresh_viable = 0.8,
                         table = FALSE) {
  stopifnot("`exposure` must be a SpatRaster object"
            = class(exposure) == "SpatRaster",
            "`exposure` layer must have values between 0-1"
            = (round(terra::minmax(exposure)[1], 0) >= 0
               && round(terra::minmax(exposure)[2], 0) <= 1),
            "`value` must be a SpatVector object"
            = class(value) == "SpatVector",
            "`t_lengths` must be a vector of three numeric values"
            = class(t_lengths) == "numeric" && length(t_lengths) == 3,
            "`interval` must be one of: 0.25, 0.5, 1, 2, 3, 4, 5, 6, 8, or 10"
            = interval %in% c(1, 2, 3, 4, 5, 6, 8, 10, 0.5, 0.25),
            "`thresh_exp` must be a numeric value between 0-1"
            = thresh_exp >= 0 && thresh_exp <= 1,
            "`thresh_viable` must be a numeric value between 0-1"
            = thresh_viable >= 0 && thresh_viable <= 1)

  names(exposure) <- "exposure"
  expl <- exposure

  stopifnot("`exposure` layer must have a CRS defined"
            = terra::crs(expl) != "",
            "`exposure` and `value` must have the same CRS"
            = terra::same.crs(expl, value))


  if (length(value) > 1) {
    value <- value[1]
    message("Value object provided has more than one feature, only the first
            point or polygon will be used.")
  }

  degs <- seq(0, 359, interval) + interval

  num_transects <- length(degs)

  wgs <- terra::project(value, "EPSG:4326")
  if (terra::geomtype(value) == "points") {
    x <- as.data.frame(wgs, geom = "XY")$x
    y <- as.data.frame(wgs, geom = "XY")$y
    # Table of start points
    linestart <- data.frame(deg = degs) %>%
      dplyr::mutate(x0 = x) %>%
      dplyr::mutate(y0 = y)
  } else if (terra::geomtype(value) == "polygons") {
    x <- as.data.frame(terra::centroids(wgs), geom = "XY")$x
    y <- as.data.frame(terra::centroids(wgs), geom = "XY")$y

    linegeom0 <- data.frame(deg = degs) %>%
      dplyr::mutate(x0 = x) %>%
      dplyr::mutate(y0 = y) %>%
      dplyr::mutate(x1 = geosphere::destPoint(cbind(.data$x0, .data$y0),
                                              .data$deg, 25000)[, 1]) %>%
      dplyr::mutate(y1 = geosphere::destPoint(cbind(.data$x0, .data$y0),
                                              .data$deg, 25000)[, 2]) %>%
      dplyr::mutate(wkt = paste("LINESTRING(", .data$x0, " ", .data$y0, ", ",
                                .data$x1, " ", .data$y1, ")", sep = ""))

    transects0 <- terra::vect(linegeom0, geom = "wkt", crs = "EPSG:4326") %>%
      terra::crop(wgs)

    if (length(terra::geom(transects0)) == num_transects * 10) {
      linestart <- as.data.frame(terra::geom(transects0)) %>%
        dplyr::select("geom", x, y) %>%
        dplyr::mutate(deg = .data$geom) %>%
        dplyr::mutate(loc = rep(c(1, 0), times = num_transects)) %>%
        tidyr::pivot_wider(
          names_from = "loc",
          values_from = c(x, y),
          names_sep = ""
        ) %>%
        dplyr::select("deg", "x0", "y0")
    } else {
      stop("Polygon shape too irregular, please simplify further and try again")
    }
  } else {
    stop("value feature must be a point or polygon")
  }

  seg1length <- t_lengths[1]
  seg2length <- t_lengths[2]
  seg3length <- t_lengths[3]

  # find end points for transects
  linegeom <- linestart %>%
    dplyr::mutate(x1 = geosphere::destPoint(cbind(.data$x0, .data$y0),
                                            .data$deg, seg1length)[, 1]) %>%
    dplyr::mutate(y1 = geosphere::destPoint(cbind(.data$x0, .data$y0),
                                            .data$deg, seg1length)[, 2]) %>%
    dplyr::mutate(seg1 = paste("LINESTRING(", .data$x0, " ", .data$y0, ", ",
                               .data$x1, " ", .data$y1, ")", sep = "")) %>%
    dplyr::mutate(x2 = geosphere::destPoint(cbind(.data$x1, .data$y1),
                                            .data$deg, seg2length)[, 1]) %>%
    dplyr::mutate(y2 = geosphere::destPoint(cbind(.data$x1, .data$y1),
                                            .data$deg, seg2length)[, 2]) %>%
    dplyr::mutate(seg2 = paste("LINESTRING(", .data$x1, " ", .data$y1, ", ",
                               .data$x2, " ", .data$y2, ")", sep = "")) %>%
    dplyr::mutate(x3 = geosphere::destPoint(cbind(.data$x2, .data$y2),
                                            .data$deg, seg3length)[, 1]) %>%
    dplyr::mutate(y3 = geosphere::destPoint(cbind(.data$x2, .data$y2),
                                            .data$deg, seg3length)[, 2]) %>%
    dplyr::mutate(seg3 = paste("LINESTRING(", .data$x2, " ", .data$y2, ", ",
                               .data$x3, " ", .data$y3, ")", sep = ""))

  linegeomlong <- linegeom %>%
    dplyr::select(c("deg", "seg1", "seg2", "seg3")) %>%
    tidyr::pivot_longer(cols = c("seg1", "seg2", "seg3"),
                        names_to = "seg", values_to = "wkt")

  transects <- terra::vect(linegeomlong,
                           geom = "wkt",
                           crs = "EPSG:4326",
                           keepgeom = TRUE) %>% # draws lines with WGS
    terra::project(expl) # reprojects to match exposure layer

  # crop to extent of transects
  exp <- terra::crop(expl, terra::rescale(transects, 1.1))

  rcm <- c(0, thresh_exp, NA, thresh_exp, 1, 1)
  rcmat <- matrix(rcm, ncol = 3, byrow = TRUE)
  highexp <- terra::classify(exp, rcmat, include.lowest = TRUE)
  highexppoly <- terra::as.polygons(highexp) #convert to polygon for intersect

  if (length(highexppoly) > 0) {
    #intersect and calculate length
    inters <- terra::crop(transects, highexppoly) %>%
      tidyterra::select(-"wkt")
    interslength <- terra::perim(inters)
    intdt <- cbind(as.data.frame(inters), interslength) # append lengths to data

    transectlength <- terra::perim(transects)
    trdt <- cbind(as.data.frame(transects), transectlength)

    transects_length <- terra::merge(transects,
                                     trdt,
                                     by = c("deg", "seg", "wkt"),
                                     all = TRUE)

    transects2 <- terra::merge(transects_length,
                               intdt,
                               by = c("deg", "seg"),
                               all = TRUE) %>%
      dplyr::mutate(interslength = tidyr::replace_na(interslength, 0)) %>%
      dplyr::mutate(viable =
                      ifelse(interslength / transectlength >= thresh_viable,
                             1, 0)) %>%
      tidyterra::select(-interslength, -transectlength)
  } else {
    transects2 <- transects %>%
      dplyr::mutate(viable = 0)
  }

  if (table == TRUE) {
    return(as.data.frame(transects2))
  } else {
    transects3 <- transects2 %>%
      dplyr::select(-"wkt")
    return(transects3)
  }
}
