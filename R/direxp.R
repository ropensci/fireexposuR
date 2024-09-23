#' Conduct a directional exposure assessment
#'
#' @description `direxp()` automates the directional vulnerability assessment
#'   methods from Beverly and Forbes 2023. This function can return the
#'   assessment transects as:
#'  * a spatial feature of transect segments
#'  * a standardized map with a satellite imagery base map as a ggplot object
#'  * a standardized radial plot as a ggplot object
#'  * a table of each transect segment as a data frame
#'   The exposure layer and value layer must have a CRS defined.
#'   If a polygon is used, it must be a simple feature without holes or complex
#'   geometry. The function will not run if the polygon should be simplified
#'   further. See Forbes and Beverly 2024 (Manuscript in preparation) for data
#'   preparation advice.
#'
#' @param exposure SpatRaster (e.g. from [exposure()])
#' @param value Spatvector of value as a point or simplified polygon
#' @param plot Boolean, when `TRUE`: returns a standardized directional plot.
#'   The default is `FALSE`.
#' @param map Boolean, when `TRUE`: returns a map of the viable transects. The
#'   default is `FALSE`.
#' @param table Boolean, when `TRUE`: returns a table of viable transects. The
#'   default is `FALSE`.
#'
#' @return a SpatVector of the transects with a attributes: degree, segment,
#'   viable. Unless:
#'    * `plot = TRUE`: a standardized plot as a ggplot object
#'    * `map = TRUE`: a standardized map as a ggplot object
#'    * `table = TRUE`: a data frame with attributes
#' @export
#' @examples
#' # generate example hazard data -----------------------------
#' set.seed(0)
#' e <- c(45,55,495,505) * 10000
#' r <- terra::rast(resolution = 100, extent = terra::ext(e))
#' terra::values(r) <- sample(c(0,1), terra::ncell(r), replace = TRUE)
#' terra::crs(r) <- "EPSG:32608"
#' r <- terra::sieve(r, threshold = 50, directions = 4)
#' haz <- terra::sieve(r, threshold = 500, directions = 4)
#' # generate an example point ---------------------------------
#' wkt <- "POINT (500000 5000000)"
#' pt <- terra::vect(wkt, crs = haz)
#' # -----------------------------------------------------------
#'
#' exp <- exposure(haz)
#'
#' # return a SpatVector that can be used in R or exported
#' direxp(exp, pt)
#'
#' # generate a map in R using a point value
#' # note: example data is drawn in the ocean so basemap is not representative
#' direxp(exp, pt, map = TRUE)



direxp <- function(exposure, value, plot = FALSE, map = FALSE, table = FALSE) {
  stopifnot("`exposure` must be a SpatRaster object"
            = class(exposure) == "SpatRaster")
  stopifnot("`value` must be a SpatVector object"
            = class(value) == "SpatVector")
  stopifnot("only one of `table`, `plot`, or `map` can be set to `TRUE`"
            = (sum(c(map, table, plot)) <= 1))
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
          names_from = .data$loc,
          values_from = c(x, y),
          names_sep = ""
        ) %>%
        dplyr::select(.data$deg, .data$x0, .data$y0)
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
    dplyr::select(c(.data$deg, .data$to5, .data$to10, .data$to15)) %>%
    tidyr::pivot_longer(cols = c(.data$to5, .data$to10, .data$to15),
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

  #intersect and calculate length
  inters <- terra::crop(transects, highexppoly) %>%
    tidyterra::select(-.data$wkt)
  interslength <- terra::perim(inters)
  intdt <- cbind(as.data.frame(inters), interslength) # append lengths to data

  transects2 <- terra::merge(transects,
                             intdt,
                             by = c("deg", "seg"),
                             all = TRUE) %>%
    dplyr::mutate(interslength = tidyr::replace_na(interslength, 0)) %>%
    dplyr::mutate(viable = ifelse(interslength / 5000 >= 0.8, 1, 0)) %>%
    tidyterra::select(-interslength)

  if (table == TRUE) {
    return(as.data.frame(transects2))
  } else if (map == TRUE) {
    #prepare for plotting

    t <- tidyterra::filter(transects2, .data$viable == 1) %>%
      terra::project("EPSG:3857")

    e <- transects2 %>%
      terra::project("EPSG:3857") %>%
      terra::rescale(1.5)

    tile <- maptiles::get_tiles(e, "Esri.WorldImagery",
                                crop = TRUE, zoom = 11)

    v <- terra::project(value, "EPSG:3857")

    cred <- maptiles::get_credit("Esri.WorldImagery")

    caption <- paste("Basemap",
                     substr(cred, 1, 63),
                     "\n",
                     substr(cred, 63, nchar(cred)))

    plt <- ggplot2::ggplot() +
      tidyterra::geom_spatraster_rgb(data = tile, alpha = 0.9) +
      tidyterra::geom_spatvector(data = t,
                                 ggplot2::aes(color = factor(.data$seg))) +
      ggplot2::scale_color_manual(
        values = c(
          "to5" = "darkred",
          "to10" = "darkorange",
          "to15" = "yellow"
        ),
        limits = c("to5", "to10", "to15"),
        breaks = c("to5", "to10", "to15"),
        labels = c("Origin to 5 km", "5 km to 10 km", "10 km to 15 km"),
        drop = FALSE
      ) +
      tidyterra::geom_spatvector(
        data = v,
        fill = NA,
        colour = "black",
        linewidth = 0.7
      ) +
      ggspatial::annotation_scale(location = "bl") +
      ggspatial::annotation_north_arrow(
        location = "bl",
        which_north = TRUE,
        pad_y = grid::unit(0.3, "in"),
        height = grid::unit(0.3, "in"),
        width = grid::unit(0.3, "in")
      ) +
      ggplot2::theme_void() +
      ggplot2::labs(
        title = "Directional Exposure",
        subtitle = "Map generated with fireexposuR()",
        color = "Segment",
        caption = caption
      ) +
      ggplot2::coord_sf(expand = FALSE)
    return(plt)
  } else if (plot == TRUE) {
    dffinal <- as.data.frame(transects2) %>%
      dplyr::select(-"wkt") %>%
      tidyr::pivot_wider(names_from = "seg", values_from = "viable") %>%
      dplyr::mutate(in15 = .data$to15) %>%
      dplyr::mutate(blank15 = 1) %>%
      dplyr::mutate(in10 = .data$to10 * 2 / 3) %>%
      dplyr::mutate(blank10 = 2 / 3) %>%
      dplyr::mutate(in5 = .data$to5 * 1 / 3) %>%
      dplyr::mutate(blank5 = 1 / 3)


    caption <- ifelse(terra::geomtype(value) == "points",
                      "Centre of plot represents point feature",
                      strwrap("centre of plot represents all transect origins
                              along the boundary of the polygon"))


    plt <- ggplot2::ggplot(data = dffinal, ggplot2::aes(x = .data$deg)) +
      ggplot2::geom_bar(
        ggplot2::aes(y = .data$blank15),
        stat = "identity",
        width = 1,
        fill = "white",
        color = "white"
      ) +
      ggplot2::geom_bar(
        ggplot2::aes(y = .data$in15),
        stat = "identity",
        width = 1,
        fill = "yellow",
        color = "yellow"
      ) +
      ggplot2::geom_bar(
        ggplot2::aes(y = .data$blank10),
        stat = "identity",
        width = 1,
        fill = "white",
        color = "white"
      ) +
      ggplot2::geom_bar(
        ggplot2::aes(y = .data$in10),
        stat = "identity",
        width = 1,
        fill = "darkorange",
        color = "darkorange"
      ) +
      ggplot2::geom_bar(
        ggplot2::aes(y = .data$blank5),
        stat = "identity",
        width = 1,
        fill = "white",
        color = "white"
      ) +
      ggplot2::geom_bar(
        ggplot2::aes(y = .data$in5),
        stat = "identity",
        width = 1,
        fill = "darkred",
        color = "darkred"
      ) +
      ggplot2::geom_line(ggplot2::aes(y = 1 / 3),
                         linewidth = 0.5, color = "black") +
      ggplot2::geom_line(ggplot2::aes(y = 2 / 3),
                         linewidth = 0.5, color = "black") +
      ggplot2::geom_line(ggplot2::aes(y = 1),
                         linewidth = 0.5, color = "black") +
      ggplot2::geom_point(ggplot2::aes(y = 0)) +
      ggplot2::annotate(
        "label",
        x = 0,
        y = 1 / 3,
        label = "5 km",
        size = 3
      ) +
      ggplot2::annotate(
        "label",
        x = 0,
        y = 2 / 3,
        label = "10 km",
        size = 3
      ) +
      ggplot2::annotate(
        "label",
        x = 0,
        y = 1,
        label = "15 km",
        size = 3
      ) +
      ggplot2::coord_polar() +
      ggplot2::ylim(0, 1) +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        panel.background =  ggplot2::element_rect(fill = "transparent"),
        plot.background = ggplot2::element_rect(fill = "transparent",
                                                color = NA),
        axis.text.x = ggplot2::element_text(
          color = "black",
          size = 15,
          face = "bold"
        )
      ) +
      ggplot2::scale_x_continuous(breaks = c(90, 180, 270, 360),
                                  labels = c("E", "S", "W", "N")) +
      ggplot2::labs(title = "Directional Exposure",
                    subtitle = "Plot generated with fireexposuR()",
                    caption = caption)
    return(plt)
  } else {
    transects3 <- transects2 %>%
      dplyr::select(-.data$wkt)
    return(transects3)
  }

}
