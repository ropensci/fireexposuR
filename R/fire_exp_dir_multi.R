#' Summarize or plot directional load for multiple values
#'
#' @description `fire_exp_dir_multi()` summarizes the directional vulnerability
#' load for multiple points in a study area in a table or a plot.
#'
#' @details
#' This function summarizes multiple directional vulnerability assessments into
#' a single table or plot. The plot is based on the methods presented in
#' Beverly and Forbes 2023. For each degree, the frequency of input values with a
#' continuous pathway at that trajectory is found. This summary can be useful
#' in identifying trends in directional exposure to values within a regional area
#' of interest.
#'
#' Continuous pathways can be assessed for the full span of all three directional
#' assessment transect segments, or limited to the outer two segments with the
#' `full` parameter. If the values being assessed are variable sizes and being
#' represented as points, it is recommended this parameter remains set to `FALSE`.
#' The inner segment is sensitive to the size of the value when a point is used.
#' Adjusting the parameters for `fire_exp_dir()` is also supported. See details
#' in [`fire_exp_dir()`] for more information.
#'
#' ## References
#' Beverly JL, Forbes AM (2023) Assessing directional vulnerability to
#' wildfire. *Natural Hazards* **117**, 831-849.
#' [DOI](https://doi.org/10.1007/s11069-023-05885-3)
#'
#'
#' @param exposure SpatRaster from [`fire_exp()`]
#' @param values Spatvector of value as a point or simplified polygon
#' @param plot Boolean, when `TRUE`: returns a standardized directional plot.
#' The default is `FALSE`.
#' @param full Boolean. Ignored when `plot = FALSE`. When `TRUE`: all 3 transect
#' segments must be viable. when `FALSE`: only the segments from seg2 and seg3
#' are considered (Default)
#' @param title (Optional) String. Ignored when `plot = FALSE`. A custom title
#' for the plot. The default is `"Directional Vulnerability for Multiple Values"`
#' @param ... arguments passed to [`fire_exp_dir()`].
#'
#' @return a data.frame of the features with attributes: value featureID,
#' degree, seg1 (binary), seg2 (binary), seg3 (binary), full (binary),
#' outer (binary). Unless:
#'    * `plot = TRUE`: a standardized plot as a ggplot object
#'
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
#' # plot directional load for multiple points
#' fire_exp_dir_multi(exposure, points, plot = TRUE, interval = 10)

fire_exp_dir_multi <- function(exposure, values, plot = FALSE, full = FALSE,
                               title = "Directional Vulnerability for Multiple Values",
                               ...) {
  stopifnot("`exposure` must be a SpatRaster object"
            = class(exposure) == "SpatRaster")
  stopifnot("`exposure` layer must have values between 0-1"
            = (round(terra::minmax(exposure)[1], 0) >= 0 && round(terra::minmax(exposure)[2], 0) <= 1))
  stopifnot("`values` must be a SpatVector object of point or polygon features"
            = (class(values) == "SpatVector" &&
                 terra::geomtype(values) %in% c("points", "polygons")))
  stopifnot("`values` and `exposure` must have the same crs"
            = terra::same.crs(values, exposure) == TRUE)
  stopifnot("`title` must be a character string"
            = class(title) == "character")

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

  if (plot == TRUE) {
    dfsums <- df2 %>%
      dplyr::mutate(sum_col = ifelse(full == TRUE,
                                     full, outer)) %>%
      dplyr::group_by(.data$deg) %>%
      dplyr::summarize(freq = sum(.data$sum_col))

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
      ggplot2::labs(title = title,
                    subtitle = "Plot generated with fireexposuR()",
                    y = "Frequency")
    return(plt)
  } else {
    return(df2)
  }
}
