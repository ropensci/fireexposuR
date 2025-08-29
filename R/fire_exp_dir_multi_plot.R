#' Plot directional load for multiple values
#'
#' @description `fire_exp_dir_multi_plot()` plots the results from
#' `fire_exp_multi()`
#'
#' @details
#' The plot is based on the methods presented in Beverly and Forbes 2023. For
#' each degree, the frequency of input values with a continuous pathway at
#' that trajectory is found. This summary can be useful in identifying trends
#' in directional exposure to values within a regional area of interest.
#'
#' Continuous pathways can be shown for the full span of all three
#' directional assessment transect segments, or limited to the outer two
#' segments with the `full` parameter. If the values being assessed are variable
#' sizes and being represented as points, it is recommended this parameter
#' remains set to `FALSE`. The inner segment is sensitive to the size of the
#' value when a point is used.
#'
#' @references
#' Beverly JL, Forbes AM (2023) Assessing directional vulnerability to
#' wildfire. *Natural Hazards* **117**, 831-849.
#' \doi{10.1007/s11069-023-05885-3}
#'
#'
#' @param dir_multi data.frame from [`fire_exp_dir_multi()`]
#' @param full Boolean. Ignored when `plot = FALSE`. When `TRUE`: all 3 transect
#' segments must be viable. when `FALSE`: only the segments from seg2 and seg3
#' are considered (Default)
#' @param title (Optional) String. A custom title for the plot. The default is
#' `"Directional Vulnerability for Multiple Values"`
#'
#' @return a standardized plot as a ggplot object
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
#' dir_multi <- fire_exp_dir_multi(exposure, points, interval = 10)
#'
#' fire_exp_dir_multi_plot(dir_multi)
#'
#'

fire_exp_dir_multi_plot <- function(dir_multi, full = FALSE, title) {
  if (missing(title)) {
    title <- "Directional Vulnerability for Multiple Values"
  }

  stopifnot("`title` must be a character string"
            = class(title) == "character",
            "`dir_multi` missing required attributes. Use fire_exp_dir_multi()"
            = any(terra::names(dir_multi) %in% c("full", "outer")))

  dfsums <- dir_multi %>%
    dplyr::mutate(sum_col = ifelse(full == TRUE, full, outer)) %>%
    dplyr::group_by(.data$deg) %>%
    dplyr::summarize(freq = sum(.data$sum_col))

  axismax <- max(dfsums$freq)

  plt <- ggplot2::ggplot(dfsums, ggplot2::aes(.data$deg, .data$freq)) +
    ggplot2::geom_hline(
      yintercept = seq(0, axismax, by = 2),
      colour = "grey90",
      linewidth = 0.2
    ) +
    ggplot2::geom_vline(
      xintercept = seq(0, 359, by = 45),
      colour = "grey90",
      linewidth = 0.2
    ) +
    ggplot2::geom_bar(
      stat = "identity",
      width = 1,
      fill = "grey50",
      color = "grey50"
    ) +
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
                  subtitle = "Plot generated with {fireexposuR}",
                  y = "Frequency")
  return(plt)

}
