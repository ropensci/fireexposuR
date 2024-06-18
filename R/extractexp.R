#' Extract exposure values to features
#'
#' @description `extractexp()` extracts the underlying exposure value for each
#' feature in the values provided. Values can be provided as either points or
#' polygons, and must be singlepart features (i.e. the attribute table has one
#' row per value). If the values are polygon features the method parameter must
#' be set to "max" or "mean", the method parameter is ignored if values are
#' points. The function retuns a SpatVector with an added exposure field. If the
#' classify parameter is provided, an additional field "classexp" is added.
#'
#' Landscape classification breaks are:
#' * Low (0-20%)
#' * Moderate (20-40%)
#' * High (40-60%),
#' * Very High (60-80%)
#' * Extreme (80-100%)
#'
#' Local classification breaks are:
#' * Nil (0%)
#' * Low (>0-15%)
#' * Moderate (15-30%)
#' * High (30-45%)
#' * Extreme (45%+)
#'
#' When either summary or map are set to `TRUE`, the classify parameter is
#' required. Setting `summary = TRUE` will return a summary table of exposure by
#' the user specified classification scheme. The table reports the number of
#' values and proportions by class. Setting `map = TRUE` will return a
#' standardized map of the values with a neutral base map and values symbolized
#' by the user specified classification scheme. The ggplot object returned can
#' be further modified with ggplot functions.
#'
#' @param exposure SpatRaster from [exposure()]
#' @param values Spatvector of points or polygons
#' @param method character, either `"max"` or `"mean"`. required when values are
#'   polygons. Default is `"max"`.
#' @param classify character, either `"local"` or `"landscape"` to specify
#'   classification scheme to use.
#' @param summary boolean, if `TRUE`: returns a summary table as a data frame.
#'   Default is `FALSE`.
#' @param map boolean, if `TRUE`: returns a ggplot map of values coloured by
#'   class. Default is `FALSE`.
#'
#' @return a SpatVector object with added fields. Unless:
#'      * `summary = TRUE`: a summary table is returned as a data frame object
#'      * `map = TRUE`: a map is returned as a ggplot object
#' @seealso [exposure()], [summexp()], [ggplot()]
#' @export
#'
#' @examples
#' # EXAMPLES IN PROGRESS
#'
#'# generate example hazard data -----------------------------
#' set.seed(0)
#' e <- c(45,55,495,505) * 10000
#' r <- terra::rast(resolution = 100, extent = terra::ext(e))
#' terra::values(r) <- sample(c(0,1), terra::ncell(r), replace = TRUE)
#' terra::crs(r) <- "EPSG:32608"
#' r <- terra::sieve(r, threshold = 50, directions = 4)
#' haz <- terra::sieve(r, threshold = 500, directions = 4)
#' # generate example AOI polygon -----------------------------
#' filepath <- "extdata/builtsimpleexamplegeom.csv"
#' g <- read.csv(system.file(filepath, package = "fireexposuR"))
#' m <- as.matrix(g)
#' v <- terra::vect(m, "polygons", crs = haz)
#' # generate example point values within polygon -------------
#' pts <- terra::spatSample(v, 200)
#' # ----------------------------------------------------------
#'
#' exp <- exposure(haz)
#'
#' # summarize values by class in a table ---------------------
#' extractexp(exp, pts, classify = "local", summary = TRUE)
#'
#' # map example points with local classification -------------
#' extractexp(exp, pts, classify = "local", map = TRUE)
#'
extractexp <- function(exposure,
                       values,
                       method = c("max", "mean"),
                       classify = c("local", "landscape"),
                       summary = FALSE,
                       map = FALSE) {
  stopifnot("`exposure` must be a SpatRaster object"
            = class(exposure) == "SpatRaster")
  stopifnot("`values` must be a SpatVector object of point or polygon features"
            = (class(values) == "SpatVector" &&
                 terra::geomtype(values) %in% c("points", "polygons")))
  stopifnot("only one of `summary` or `map` can be set to `TRUE`"
            = (map == TRUE && summary == TRUE) == FALSE)
  if (missing(classify)) {
    stopifnot("must provide `classify` argument if `summary` or `map` = `TRUE`"
              = (summary == TRUE || map == TRUE) == FALSE)
  } else {
    classify <- match.arg(classify)
  }

  names(exposure) <- 'exposure'
  exp <- exposure

  if (terra::geomtype(values) == "points") {
    ext <- terra::extract(exp, values, bind = TRUE)
  } else {
    method <- match.arg(method)
    if (method == "mean") {
      ext <- terra::extract(exp, values, fun = mean, bind = TRUE) %>%
        dplyr::mutate(method = "mean exposure")
    } else {
      ext <- terra::extract(exp, values, fun = max, bind = TRUE) %>%
        dplyr::mutate(maxexp = "max exposure")
    }
  }
  if (missing(classify)) {
    return(ext)
    stop()
  }
  classify <- match.arg(classify)
  if (classify == "landscape") {
    ext <- ext %>%
      dplyr::mutate(scale = classify) %>%
      dplyr::mutate(
        classexp = dplyr::case_when(
          exposure >= 0.8 ~ 5,
          exposure >= 0.6 ~ 4,
          exposure >= 0.4 ~ 3,
          exposure >= 0.2 ~ 2,
          exposure >= 0 ~ 1
        )
      )
  }
  if (classify == "local") {
    ext <- ext %>%
      dplyr::mutate(scale = classify) %>%
      dplyr::mutate(
        classexp = dplyr::case_when(
          exposure >= 0.45 ~ 5,
          exposure >= 0.3 ~ 3,
          exposure >= 0.15 ~ 2,
          exposure > 0 ~ 1,
          exposure == 0 ~ 0
        )
      )
  }

  lut <- 0:5
  names(lut) <- c("Nil", "Low", "Moderate", "High", "Very High", "Extreme")
  ext <- ext %>%
    dplyr::mutate(class = names(lut)[match(.data$classexp, lut)])

  ext$class <- factor(ext$class, levels = names(lut))


  if (summary == TRUE) {
    df <- as.data.frame(ext) %>%
      dplyr::count(class) %>%
      dplyr::mutate(prop = .data$n / sum(.data$n)) %>%
      dplyr::select(c(.data$class, .data$n, .data$prop))
    return(df)
    if (map == TRUE) {
      stop("Only one of summary or map can be set to true")
    }
  }
  if (map == TRUE) {
    v <- terra::project(ext, "EPSG: 3857")
    e <- terra::rescale(v, 1.5)
    tile <- maptiles::get_tiles(e, "Esri.WorldGrayCanvas") %>%
      terra::crop(e)

    cols <- c(
      "Nil" = "grey",
      "Low" = "yellow",
      "Moderate" = "orange",
      "High" = "darkorange",
      "Very High" = "red",
      "Extreme" = "darkred"
    )


    plt <- ggplot2::ggplot() +
      tidyterra::geom_spatraster_rgb(data = tile, alpha = 0.8) +
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
        title = "Classified Exposure to Values",
        subtitle = "Map generated with fireexposuR()",
        caption = maptiles::get_credit("Esri.WorldGrayCanvas")
      )

    if (terra::geomtype(v) == "points") {
      plt <- plt +
        tidyterra::geom_spatvector(data = v,
                                   ggplot2::aes(color = factor(.data$class)),
                                   shape = 16) +
        ggplot2::scale_color_manual(values = cols) +
        ggplot2::labs(color = paste("Exposure Class (", classify, ")",
                                    sep = "")) +
        ggplot2::coord_sf(expand = FALSE)
    } else {
      plt <- plt +
        tidyterra::geom_spatvector(data = v,
                                   ggplot2::aes(fill = factor(.data$class)),
                                   color = NA) +
        ggplot2::scale_fill_manual(values = cols) +
        ggplot2::labs(fill = paste("Exposure Class (", classify, ")",
                                   sep = "")) +
        ggplot2::coord_sf(expand = FALSE)
    }
    return(plt)
    stop()
  } else {
    return(ext)
  }
}
