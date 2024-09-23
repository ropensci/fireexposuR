#' Generate a directional load plot or table for multiple values
#'
#' @description `multidirexp()` automates the directional vulnerability
#'   assessment methods from Beverly and Forbes 2023. This function can return
#'   directional loads as:
#'  * a standardized radial plot as a ggplot object
#'  * a table summarizing if each degree is included by feature
#'
#' @param exposure SpatRaster from [exposure()]
#' @param values Spatvector of value as a point or simplified polygon
#' @param plot Boolean, when `TRUE`: returns a standardized directional plot.
#'   The default is `FALSE`.
#' @param all Boolean, when `TRUE`: considers all 3 segments (0-15km) of
#'   directional transects. when `FALSE`: only the segments from 5-15 km are
#'   included (Default)
#'
#' @return a data.frame of the features with attributes: value featureID,
#'   degree, to5 (binary), to10(binary), t015(binary), full(binary),
#'   outer (binary). Unless:
#'      * `plot = TRUE`: a standardized plot as a ggplot object

#' @export
#'
#' @examples
#' # generate example hazard data -----------------------------
#' set.seed(0)
#' e <- c(45, 55, 495, 505) * 10000
#' r <- terra::rast(resolution = 100, extent = terra::ext(e))
#' terra::values(r) <- sample(c(0, 1), terra::ncell(r), replace = TRUE)
#' terra::crs(r) <- "EPSG:32608"
#' r <- terra::sieve(r, threshold = 50, directions = 4)
#' haz <- terra::sieve(r, threshold = 500, directions = 4)
#' # example points across the landscape ----------------------
#' e <- terra::buffer(terra::vect(terra::ext(haz), crs = haz), -15500)
#' pts <- terra::spatSample(e, 200)
#' # ----------------------------------------------------------
#'
#' exp <- exposure(haz, tdist = "l")
#' # this example will take a while to run
#' \dontrun{
#' multidirexp(exp, pts, plot = TRUE)
#' }

multidirexp <- function(exposure, values, plot = FALSE, all = FALSE) {
  stopifnot("`exposure` must be a SpatRaster object"
            = class(exposure) == "SpatRaster")
  stopifnot("`values` must be a SpatVector object of point or polygon features"
            = (class(values) == "SpatVector" &&
                 terra::geomtype(values) %in% c("points", "polygons")))
  stopifnot("`values` and `exposure` must have the same crs"
            = terra::same.crs(values, exposure) == TRUE)

  names(exposure) <- "exposure"
  expl <- exposure
  fts <- values

  # test that values features are within exposure extent before running
  buff <- terra::buffer(fts, 15000) %>%
    terra::aggregate()
  explext <- terra::classify(expl, c(-Inf,Inf,1)) %>%
    terra::as.polygons()
  stopifnot("Values features must be within extent of the exposure layer"
            = terra::relate(buff, explext, "coveredby") == TRUE)

  df <- data.frame()

  for (i in 1:length(fts)) {
    dat <- direxp(expl, fts[i], table = TRUE) %>%
      dplyr::select(-"wkt") %>%
      dplyr::mutate(featureID = i) %>%
      tidyr::pivot_wider(names_from = "seg", values_from = "viable") %>%
      dplyr::select("featureID", tidyselect::everything())
    df <- rbind(df, dat)
  }

  df2 <- df %>%
    dplyr::mutate(full = ifelse(.data$to5 + .data$to10 + .data$to15 == 3,
                                1, 0)) %>%
    dplyr::mutate(outer = ifelse(.data$to10 + .data$to15 == 2, 1, 0))

  if (plot == TRUE) {
    dfsums <- df2 %>%
      dplyr::group_by(.data$deg) %>%
      dplyr::summarise(freq = ifelse(all == TRUE,
                                     sum(.data$full), sum(.data$outer)))
    axismax <- max(dfsums$freq)

    plt <- ggplot2::ggplot(dfsums, ggplot2::aes(.data$deg, .data$freq)) +
      ggplot2::geom_hline(yintercept = seq(0, axismax, by = 2),
                          colour = "grey90", linewidth = 0.2) +
      ggplot2::geom_vline(xintercept = seq(0, 359, by = 45),
                          colour = "grey90", linewidth = 0.2) +
      ggplot2::geom_bar(stat = "identity",
                        width = 1,
                        fill = "grey50",
                        color = "grey50") +
      ggplot2::coord_polar() +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(
          color = "black",
          size = 15,
          face = "bold"
        ),
        axis.title.y = ggplot2::element_text(hjust = 0.75, vjust = 3),
        axis.title.x = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank()
      )  +
      ggplot2::scale_y_continuous(breaks = seq(0, axismax, by = 2)) +
      ggplot2::scale_x_continuous(breaks = c(90, 180, 270, 360),
                                  labels = c("E", "S", "W", "N")) +
      ggplot2::labs(title = "Directional Exposure Load for Multiple Values",
                    subtitle = "Plot generated with fireexposuR()",
                    y = "Frequency")
    return(plt)
  } else {
    return(df2)
  }
}
