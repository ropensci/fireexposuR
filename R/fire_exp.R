#' Compute the wildfire exposure metric
#'
#' @description
#' `fire_exp()` returns a SpatRaster of wildfire exposure values calculated
#' using the input hazard fuel raster for a specified transmission distance.
#'
#' @details
#' This function is the primary function in this package, the output from this
#' function serves as the first input to all other functions in this package.
#'
#' ## Background
#'
#' Wildfire exposure was first proposed for community scale assessments by
#' Beverly et al. (2010). Wildfire entry points to the built environment were
#' identified by evaluating the surrounding wildland fuels as potential ignition
#' sources. These methods were adapted and validated by Beverly et al. (2021)
#' for landscape scale assessments. Visual aids to better understand these
#' concepts are available in the Wildfire Exposure Assessment Guidebook
#' (FireSmart Canada 2018) which includes intuitive illustrations.
#'
#' Three transmission distances were defined in Beverly et al. (2010) for
#' different scales of wildfire ignition processes. Different fuel types
#' are capable of transmitting fire at different scales. The transmission
#' distances define a potential maximum spread distance from an ignition
#' source. If the default distances do not accurately represent fire behaviour
#' in your area of interest you can adjust them with [fire_exp_adjust()].
#'
#' ### Radiant heat
#'
#' The default transmission distance for radiant heat is 30 meters. At this
#' scale of wildfire transmission wildfire can spread from direct flame contact
#' or thermal radiation warming of fuels.
#'
#' ### Ember spotting
#'
#' Embers (AKA firebrands) can be carried by wind beyond the active fire front.
#' To determin two different ember spreading distances Beverly et al. (2010)
#' reviewed fire-spot distance observations and applied predictive models to
#' verify these distances were a reasonable assumption.
#'
#' #### Short-range embers
#'
#' The default transmission distance for short-range embers is 100 meters. Some
#' fuel types have the potential to produce embers over short distances. An
#' example fuel type that has the potential to transmit short-range embers is
#' a deciduous forest. Fuels considered hazardous to short-range ember ignition
#' will also be hazardous to transmit fire via radiant heat or direct flame
#' contact.
#'
#' #### Long-range embers
#'
#' The default transmission distance for long-range embers is 500 meters.
#' Some fuel types have the potential to transmit embers across large distances.
#' This landscape scale process is often a contributor to large, fast-moving
#' fires. Any fuels that are considered hazardous for long-range ember spotting
#' could also transmit fire by short-range ember spotting and radiant heat. An
#' example fuel type that has the potential to transmit long-range embers is a
#' mature pine stand.
#'
#' Although it is possible for embers from certain fuel types to be carried
#' much farther than the defined 500 meter transmission distance under extreme
#' conditions, these cases are relatively uncommon.
#'
#' The 500 meter spread distance has been further validated across Alberta
#' (Beverly et al. 2021), in Alaska (Schmidt et al. 2024), across the entire
#' Canadian landbase (manuscript in preparation), and in Portugal (manuscript
#' in preparation).
#'
#' ## Technical
#'
#' ### How the metric is calculated
#'
#' The wildfire exposure metric is calculated for each cell individually using a
#' focal window of the surrounding cells within the specified transmission
#' distance. The proportion of cells within the assessment window that are
#' classed as hazardous is returned. The wildfire exposure metric has a range
#' of 0-1. A wildfire exposure value of 0.5 can be interpreted as 50% of cells
#' within the specified transmission distance area have the potential to
#' spread fire to the assessment cell. A value of 0.5 can also be interpreted
#' in the other direction, a fire in the assessment cell could potentially
#' spread to 50% of the surrounding cells within the specified transmission
#' distance.
#'
#' ### Input features
#'
#' An input hazard raster must be prepared by the user in accordance with the
#' intended use. First, refer to the Get Started vignette by running
#' `vignette("fireexposuR")` to determine what the data requirements are for
#' the intended application. Then refer to the Preparing Input Data vignette by
#' running `vignette("prep-input-data")` for lots of recommendations, advice,
#' and examples.
#'
#' A separate hazard raster should be prepared for each of the transmission
#' distances of interest. There are also minimum spatial resolution and extent
#' requirements for each transmission distances.
#'
#' Long-range embers:
#' -  minimum raster resolution is 150 meters
#' -  The dimensions of the data must be wider/taller than 1000 meters because
#' 500 meters of data will be lost along the perimeter due to edge effects
#'
#' Short-range embers:
#' -  minimum raster resolution is 33 meters
#' -  The dimensions of the data must be wider/taller than 200 meters because
#' 100 meters of data will be lost along the perimeter due to edge effects
#'
#' Radiant heat:
#' -  minimum raster resolution is 10 meters
#' -  The dimensions of the data must be wider/taller than 60 meters because
#' 30 meters of data will be lost along the perimeter due to edge effects
#'
#' ### Spatial Reference
#'
#' The exposure raster will be returned in the same CRS as the input hazard
#' layer. A crs must be defined if the outputs will be used in other functions
#' in this package.
#'
#' @references
#' Beverly JL, McLoughlin N, Chapman E (2021) A simple metric of landscape fire
#' exposure. *Landscape Ecology* **36**, 785-801.
#' \doi{10.1007/s10980-020-01173-8}
#'
#' Beverly JL, Bothwell P, Conner JCR, Herd EPK (2010) Assessing the exposure
#' of the built environment to potential ignition sources generated from
#' vegetative fuel. *International Journal of Wildland Fire* **19**, 299-313.
#' \doi{10.1071/WF09071}
#'
#' FireSmart Canada (2018) Wildfire exposure assessment guidebook. Available
#' [here](https://firesmartcanada.ca/wp-content/uploads/2022/01/FS_ExposureAssessment_Sept2018-1.pdf)
#'
#' Hijmans R (2024). _terra: Spatial Data Analysis_. R package version 1.7-78,
#' [CRAN](https://CRAN.R-project.org/package=terra).
#'
#' Schmidt JI, Ziel RH, Calef MP, Varvak A (2024) Spatial distribution of
#' wildfire threat in the far north: exposure assessment in boreal communities.
#' *Natural Hazards* **120**, 4901-4924.
#' \doi{10.1007/s11069-023-06365-4}
#'
#'
#' @param hazard a SpatRaster that represents hazardous fuels for the
#'   transmission distance specified in tdist
#' @param tdist a character vector, can be: `"l"` for long-range
#'   embers (Default), `"s"` for short-range embers or, `"r"` for radiant heat
#' @param no_burn (optional) a SpatRaster that represents the non-burnable
#'   landscape. Any cells that cannot receive wildfire (e.g. open water, rock)
#'   and any cells that are not natural (e.g. built environment,
#'   irrigated agricultural areas) should be of value 1, all other cells
#'   should be NODATA. This parameter should be provided if preparing data
#'   for [fire_exp_validate()]
#'
#' @return A SpatRaster object of exposure values between 0-1
#'
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' # read example hazard data
#' hazard_file_path <- "extdata/hazard.tif"
#' hazard <- terra::rast(system.file(hazard_file_path, package = "fireexposuR"))
#'
#' # compute long range exposure
#' fire_exp(hazard, tdist = "l")
#'

fire_exp <- function(hazard, tdist = c("l", "s", "r"), no_burn) {
  stopifnot("`hazard` must be a SpatRaster object"
            = class(hazard) == "SpatRaster",
            "`hazard` layer must have values between 0-1"
            = (round(terra::minmax(hazard)[1]) >= 0
               && round(terra::minmax(hazard)[2] <= 1)))
  tdist <- match.arg(tdist)

  if (terra::crs(hazard) == "") {
    message("Input CRS is undefined: If output will be used in
            other fireexposuR functions a CRS must be defined")
  }

  if (!missing(no_burn)) {
    stopifnot("`no_burn` must be a SpatRaster object"
              = class(no_burn) == "SpatRaster",
              "`no_burn` and `hazard` must have same CRS"
              = terra::same.crs(hazard, no_burn),
              "`no_burn` must only contain values of 1 or NA"
              = unique(terra::values(no_burn) %in% c(1, NA, NaN)),
              "`no_burn` extent must be within `hazard` extent"
              = terra::relate(no_burn, hazard, "within"))
  }

  haz <- hazard
  res <- terra::res(haz)[1]

  if (tdist == "l") {
    stopifnot("Insufficient resolution for longrange ember exposure assessment"
              = res <= 150)
    annulus <- c(res, 500)
    window <- MultiscaleDTM::annulus_window(annulus, "map", res)
  }
  if (tdist == "s") {
    stopifnot("Insufficient resolution for shortrange ember exposure assessment"
              = res <= 33)
    annulus <- c(res, 100)
    window <- MultiscaleDTM::annulus_window(annulus, "map", res)
  }
  if (tdist == "r") {
    stopifnot("Insufficient resolution for radiant heat exposure assessment"
              = res <= 10)
    annulus <- c(res, 30)
    window <- MultiscaleDTM::annulus_window(annulus, "map", res)
  }
  stopifnot("Extent of hazard raster too small for exposure assessment"
            = terra::nrow(window) * 2 < terra::nrow(haz))
  wgtwindow <- window / sum(window, na.rm = TRUE)
  exp <- terra::focal(haz, wgtwindow, fun = sum) %>%
    tidyterra::rename(exposure = "focal_sum")
  if (missing(no_burn)) {
    return(exp)
  } else {
    expb <- terra::mask(exp, no_burn, inverse = TRUE)
    return(expb)
  }
}
