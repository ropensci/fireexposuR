#' Conduct a directional exposure assessment
#'
#' @description `fire_exp_dir()` automates the directional vulnerability assessment
#'   methods from Beverly and Forbes 2023. This function can return the
#'   assessment transects as a spatial feature of transect segments or a table
#'   of each transect segment as a data frame
#'   The exposure layer and value layer must have a CRS defined.
#'   If a polygon is used, it must be a simple feature without holes or complex
#'   geometry. The function will not run if the polygon should be simplified
#'   further. See Forbes and Beverly 2024 (Manuscript in preparation) for data
#'   preparation advice.
#'
#' @param exposure SpatRaster (e.g. from [fire_exp()])
#' @param value Spatvector of value as a point or simplified polygon
#' @param table Boolean, when `TRUE`: returns a table instead of a feature. The
#'   default is `FALSE`.
#'
#' @return a SpatVector of the transects with a attributes: degree, segment,
#'   viable. Unless:
#'    * `table = TRUE`: a data frame
#' @export
#' @examples
#' # read example hazard data ----------------------------------
#' filepath <- "extdata/hazard.tif"
#' haz <- terra::rast(system.file(filepath, package = "fireexposuR"))
#' # generate an example point ---------------------------------
#' wkt <- "POINT (500000 5000000)"
#' pt <- terra::vect(wkt, crs = haz)
#' # -----------------------------------------------------------
#'
#' exp <- fire_exp(haz)
#'
#' fire_exp_dir(exp, pt)
#'



fire_exp_dir <- function(exposure, value, table = FALSE) {
  stopifnot("`exposure` must be a SpatRaster object"
            = class(exposure) == "SpatRaster")
  stopifnot("`value` must be a SpatVector object"
            = class(value) == "SpatVector")
  names(exposure) <- "exposure"
  expl <- exposure

  if (length(value) > 1) {
    value <- value[1]
    message("Value object provided has more than one feature, only the first
            point or polygon will be used.")
  }

  wgs <- terra::project(value, "EPSG:4326")
  if (terra::geomtype(value) == "points") {
    x <- as.data.frame(wgs, geom = "XY")$x
    y <- as.data.frame(wgs, geom = "XY")$y
    # Table of start points
    linestart <- data.frame(deg = 1:360) %>%
      dplyr::mutate(x0 = x) %>%
      dplyr::mutate(y0 = y)
  } else if (terra::geomtype(value) == "polygons") {
    x <- as.data.frame(terra::centroids(wgs), geom = "XY")$x
    y <- as.data.frame(terra::centroids(wgs), geom = "XY")$y

    linegeom0 <- data.frame(deg = 1:360) %>%
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

    if (length(terra::geom(transects0)) == 3600) {
      linestart <- as.data.frame(terra::geom(transects0)) %>%
        dplyr::select("geom", x, y) %>%
        dplyr::mutate(deg = .data$geom) %>%
        dplyr::mutate(loc = rep(c(1, 0), times = 360)) %>%
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

  # find end points for transects
  linegeom <- linestart %>%
    dplyr::mutate(x5 = geosphere::destPoint(cbind(.data$x0, .data$y0),
                                            .data$deg, 5000)[, 1]) %>%
    dplyr::mutate(y5 = geosphere::destPoint(cbind(.data$x0, .data$y0),
                                            .data$deg, 5000)[, 2]) %>%
    dplyr::mutate(to5 = paste("LINESTRING(", .data$x0, " ", .data$y0, ", ",
                              .data$x5, " ", .data$y5, ")", sep = "")) %>%
    dplyr::mutate(x10 = geosphere::destPoint(cbind(.data$x5, .data$y5),
                                             .data$deg, 5000)[, 1]) %>%
    dplyr::mutate(y10 = geosphere::destPoint(cbind(.data$x5, .data$y5),
                                             .data$deg, 5000)[, 2]) %>%
    dplyr::mutate(to10 = paste("LINESTRING(", .data$x5, " ", .data$y5, ", ",
                               .data$x10, " ", .data$y10, ")", sep = "")) %>%
    dplyr::mutate(x15 = geosphere::destPoint(cbind(.data$x10, .data$y10),
                                             .data$deg, 5000)[, 1]) %>%
    dplyr::mutate(y15 = geosphere::destPoint(cbind(.data$x10, .data$y10),
                                             .data$deg, 5000)[, 2]) %>%
    dplyr::mutate(to15 = paste("LINESTRING(", .data$x10, " ", .data$y10, ", ",
                               .data$x15, " ", .data$y15, ")", sep = ""))

  linegeomlong <- linegeom %>%
    dplyr::select(c("deg", "to5", "to10", "to15")) %>%
    tidyr::pivot_longer(cols = c("to5", "to10", "to15"),
                        names_to = "seg", values_to = "wkt")

  transects <- terra::vect(linegeomlong,
                           geom = "wkt",
                           crs = "EPSG:4326",
                           keepgeom = TRUE) %>% # draws lines with WGS
    terra::project(expl) # reprojects to match exposure layer

  # crop to extent of transects
  exp <- terra::crop(expl, terra::rescale(transects, 1.1))

  rcm <- c(0, 0.6, NA, 0.6, 1, 1)
  rcmat <- matrix(rcm, ncol = 3, byrow = TRUE)
  highexp <- terra::classify(exp, rcmat, include.lowest = TRUE)
  highexppoly <- terra::as.polygons(highexp) #convert to polygon for intersect

  if (length(highexppoly) > 0){
    #intersect and calculate length
    inters <- terra::crop(transects, highexppoly) %>%
      tidyterra::select(-"wkt")
    interslength <- terra::perim(inters)
    intdt <- cbind(as.data.frame(inters), interslength) # append lengths to data
    transects2 <- terra::merge(transects,
                               intdt,
                               by = c("deg", "seg"),
                               all = TRUE) %>%
      dplyr::mutate(interslength = tidyr::replace_na(interslength, 0)) %>%
      dplyr::mutate(viable = ifelse(interslength / 5000 >= 0.8, 1, 0)) %>%
      tidyterra::select(-interslength)
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
