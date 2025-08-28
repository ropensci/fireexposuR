#' Visualize validation outputs in a plot
#'
#' @description `fire_exp_validate_plot()` Visualizes the results from
#' `fire_exp_validate()` in a bar plot
#'
#' @seealso [fire_exp_validate()]
#'
#' @param validation_table The output table from [fire_exp_validate()]
#' @param what string. Which plot should be returned? Can be `"sample"`,
#'  `"total"`,or `"both"` (Default)
#' @param title Optional. String. Add a custom title to the plot.
#'
#' @return a ggplot object
#'
#' @export
#'
#' @examples
#' # read example hazard data
#' hazard_file_path <- "extdata/hazard.tif"
#' hazard <- terra::rast(system.file(hazard_file_path, package = "fireexposuR"))
#'
# generate example non-burnable cells data
#' polygon_path <- system.file("extdata", "polygon.shp", package ="fireexposuR")
#' polygon <- terra::vect(polygon_path)
#' no_burn <- terra::rasterize(polygon, hazard)
#'
#' # generate example fire polygons by buffering random points
#' points <- terra::spatSample(terra::rescale(hazard, 0.8),
#'                             30, as.points = TRUE)
#' fires <- terra::buffer(points, 800)

#' # PLEASE NOTE THIS EXAMPLE DATA DOES NOT GENERATE MEANINGFUL RESULTS
#'
#' # compute exposure and remove non-burnable cells
#' exposure <- fire_exp(hazard, no_burn = no_burn)
#'
#' # results as table
#' validation_outputs <- fire_exp_validate(exposure, fires)
#'
#' # results as bar chart
#' fire_exp_validate_plot(validation_outputs)
#'

fire_exp_validate_plot <- function(validation_table,
                                   what = c("both", "total", "sample"),
                                   title) {
  props <- validation_table

  what <- match.arg(what)

  stopifnot("`validation_table` does not have expected attributes.
            Use output from `fire_exp_validate()`"
            = any(names(props) %in% c("exposure", "of", "prop")))

  props <- dplyr::arrange(props, dplyr::desc(.data$group))

  props$exposure <- as.factor(props$exposure)

  labs <- unique(props$exp_vals)

  if (what == "sample") {
    props <- dplyr::filter(props, .data$of == "Sample")
  }

  if (what == "total") {
    props <- dplyr::filter(props, .data$of == "Total")
  }

  plt <- ggplot2::ggplot(props, ggplot2::aes(x = .data$exposure,
                                             y = .data$prop,
                                             fill = .data$group,
                                             color = .data$group)) +
    ggplot2::geom_col(position = "identity", linetype = 2, linewidth = 0.8) +
    ggplot2::facet_grid(~.data$of) +
    ggplot2::scale_fill_manual(values = c("transparent", "gray")) +
    ggplot2::scale_color_manual(values = c("black", "transparent")) +
    ggplot2::scale_y_continuous(expand =
                                  ggplot2::expansion(mult = c(0, 0.1))) +
    ggplot2::scale_x_discrete(name = "Exposure Class",
                              labels = labs) +
    ggplot2::labs(y = "Proportion") +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 90,
                                                       vjust = 0.5,
                                                       hjust = 1)) +
    ggplot2::labs(
      caption = "Plot generated with fireexposuR()"
    )

  if (!missing(title)) {
    plt <- plt +
      ggplot2::labs(title = title)
  }

  return(plt)
}
