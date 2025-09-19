#' Validate exposure with observed fires
#'
#' @description For advanced users. `fire_exp_validate()` compares the
#'   proportion of exposure classes in a the study area to the proportion of
#'   exposure classes within observed burned areas.
#'
#' @details
#' This function automates a simple validation method to assess if fire burns
#' preferentially in areas with high exposure. The methods, and figure produced
#' with [`fire_exp_validate_plot()`], are based on Beverly et al. (2021).
#'
#' The function requires an exposure raster produced for a past point in time.
#' Cells that cannot burn, or do not represent natural land cover should be
#' removed by setting the `no_burn` parameter in [fire_exp()] or
#' [fire_exp_adjust()].
#'
#' The function also requires fire perimeter data. Currently, the function takes
#' the fires as a Vector of polygons because that is typically how fire
#' boundaries are stored in spatial databases. The fires input data should
#' include all of the burned area that has occurred following the time period
#' the input exposure layer was produced for. It is up to the user to determine
#' the appropriate amount of burned area required for a meaningful assessment.
#'
#' A random sample is taken to account for spatial autocorrelation, the
#' sampled location results can be used to test for significant differences.
#' The sample size can be adjusted. The sample size represents a proportion of
#' cells, the default is `0.005` (0.5%). It is the user's responsibility to set
#' an appropriate sample size.
#'
#' The class breaks can be customized from the default of 0.2 intervals by
#' setting the `class_breaks` parameter. A class of Nil is automatically added
#' for values exactly equal to 0.
#'
#'
#' @references
#' Beverly JL, McLoughlin N, Chapman E (2021) A simple metric of landscape
#' fire exposure. *Landscape Ecology* **36**, 785-801.
#' \doi{10.1007/s10980-020-01173-8}
#'
#' @seealso [fire_exp_validate_plot()]
#'
#' @param burnableexposure A SpatRaster of exposure, non-burnable cells should
#'    be removed using optional parameter `no_burn = `in [fire_exp()].
#' @param fires A SpatVector of observed fire perimeters
#' @param aoi (Optional) A SpatVector that delineates an area of interest
#' @param class_breaks (Optional) vector of numeric values between 0-1 of the
#' upper limits of each class. The default is `c(0.2, 0.4, 0.6, 0.8, 1)`. See
#' details.
#' @param samplesize Proportion of areas to sample. The default is `0.005`
#' (0.5%)
#'
#' @return a table of number of cells (n) and proportions (prop) of exposure
#' classes within a sampled area (Sample) and across the full extent (Total).for
#' the full extent of the exposure data (expected) and only within the burned
#' areas (observed).
#'
#' @export
#'
#' @examples
#' # read example hazard data
#' hazard_file_path <- "extdata/hazard.tif"
#' hazard <- terra::rast(system.file(hazard_file_path, package = "fireexposuR"))
#'
#' # generate example non-burnable cells data
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
#' # validation table
#' fire_exp_validate(exposure, fires)
#'

fire_exp_validate <- function(burnableexposure, fires, aoi,
                              class_breaks = c(0.2, 0.4, 0.6, 0.8, 1),
                              samplesize = 0.005) {
  names(burnableexposure) <- "exposure"
  expb <- burnableexposure
  stopifnot(
    "`burnableexposure` must be a SpatRaster object"
    = class(expb) == "SpatRaster",
    "Linear units of `exposure` layer must be in meters"
    = terra::linearUnits(expb) == 1,
    "`exposure` layer must have values between 0-1"
    = (round(terra::minmax(expb)[1], 0)) >= 0
    && round(terra::minmax(expb)[2], 0) <= 1,
    "`fires` must be a SpatVector object"
    = class(fires) == "SpatVector",
    "`burnableexposure` and `fires` must have same CRS"
    = terra::same.crs(burnableexposure, fires)
  )
  if (!missing(aoi)) {
    stopifnot(
      "`aoi` must be a SpatVector object"
      = class(aoi) == "SpatVector",
      "`burnableexposure` and `aoi` must have same CRS"
      = terra::same.crs(burnableexposure, aoi)
    )
  }

  class_breaks <- sort(class_breaks)

  # class_breaks checks
  stopifnot(
    "`class_breaks` must be a vector of numbers"
    = class(class_breaks) == "numeric",
    "`class_breaks` must have 1 as the maximum value"
    = max(class_breaks) == 1,
    "`class_breaks` must be greater than 0"
    = class_breaks > 0
  )

  class_labels <- character()

  label_breaks <- c(0, class_breaks)
  for (i in seq_along(label_breaks)) {
    class_labels[i] <- paste(label_breaks[i], "-", label_breaks[i + 1])
  }

  class_labels <- c("Nil", utils::head(class_labels, -1))

  lut <- data.frame(start = c(0, 0, utils::head(class_breaks, -1)),
                    end = c(0, class_breaks),
                    factor = 0:length(class_breaks),
                    label = class_labels)

  rcmats <- as.matrix(lut[, 1:3])

  classexp <- terra::classify(expb, rcmats, include.lowest = TRUE)

  if (missing(aoi)) {
    studyarea <- classexp
  } else {
    studyarea <- terra::crop(classexp, aoi, overwrite = TRUE) %>%
      terra::mask(aoi)
  }

  firesarea <- studyarea * terra::rasterize(fires, studyarea)

  df1 <- dplyr::count(as.data.frame(studyarea), .data$exposure) %>%
    dplyr::mutate(of = "Total") %>%
    dplyr::mutate(group = "Expected") %>%
    dplyr::mutate(prop = .data$n / sum(.data$n))


  df2 <- dplyr::count(as.data.frame(firesarea), .data$exposure) %>%
    dplyr::mutate(of = "Total") %>%
    dplyr::mutate(group = "Observed") %>%
    dplyr::mutate(prop = .data$n / sum(.data$n))

  samplestudyareasize <- round(sum(df1$n) * samplesize)
  samplefiresareasize <- round(sum(df2$n) * samplesize)

  props <- rbind(df1, df2)

  df3 <- dplyr::count(terra::spatSample(studyarea,
                                        samplestudyareasize,
                                        na.rm = TRUE,
                                        as.df = TRUE,
                                        method = "random"),
                      .data$exposure) %>%
    dplyr::mutate(of = "Sample") %>%
    dplyr::mutate(group = "Expected") %>%
    dplyr::mutate(prop = .data$n / sum(.data$n))

  props <- rbind(props, df3)

  df4 <- dplyr::count(terra::spatSample(firesarea,
                                        samplefiresareasize,
                                        na.rm = TRUE,
                                        as.df = TRUE,
                                        method = "random"),
                      .data$exposure) %>%
    dplyr::mutate(of = "Sample") %>%
    dplyr::mutate(group = "Observed") %>%
    dplyr::mutate(prop = .data$n / sum(.data$n))

  lut2 <- as.factor(lut$factor)

  names(lut2) <- lut$label

  props <- rbind(props, df4) %>%
    dplyr::mutate(exp_vals = names(lut2)[match(.data$exposure, lut2)]) %>%
    dplyr::select("exposure", "exp_vals",
                  "of", "group", "n", "prop")

  return(props)
}
