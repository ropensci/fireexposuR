
validateexp <- function(expb, fires, aoi){
  rcmat <- matrix(c(0,0.2,1,
                    0.2,0.4,2,
                    0.4,0.6,3,
                    0.6,0.8, 4,
                    0.8,1, 5), ncol=3, byrow = T)

  classexp <- terra::classify(expb, rcmat, include.lowest = T)

  if (missing(aoi)){
    studyarea <- classexp
    studyareafires <- fires
  } else {
    studyarea <- terra::crop(classexp, aoi, overwrite = T) %>%
      terra::mask(aoi)
    studyareafires <- terra::crop(fires, aoi)
  }

  firesarea <- terra::crop(studyarea, studyareafires, overwrite = T) %>%
    terra::mask(studyareafires)


  totalstudyarea <- dplyr::count(as.data.frame(studyarea), .data$exposure) %>%
    dplyr::mutate(proptotstudyarea = .data$n/sum(.data$n))
  totalfirearea <- dplyr::count(as.data.frame(firesarea), .data$exposure) %>%
    dplyr::mutate(proptotfiresarea = .data$n/sum(.data$n)) %>%
    dplyr::left_join(totalstudyarea, by = "exposure")


  samplestudyareasize <- round(sum(totalstudyarea$n)*0.001)
  samplefiresareasize <- round(sum(totalfirearea$n.x)*0.001)

  samplestudyarea <- dplyr::count(terra::spatSample(studyarea, samplestudyareasize, na.rm = T, as.df = T, method = 'random'), .data$exposure) %>%
    dplyr::mutate(propsampstudyarea = .data$n/sum(.data$n)) %>%
    dplyr::select(-.data$n) %>%
    dplyr::left_join(totalfirearea, by = "exposure")

  samplefiresarea <- dplyr::count(terra::spatSample(firesarea, samplefiresareasize, na.rm = T, as.df = T, method = 'random'), .data$exposure) %>%
    dplyr::mutate(propsampfiresarea = .data$n/sum(.data$n)) %>%
    dplyr::select(-.data$n) %>%
    dplyr::left_join(samplestudyarea, by = "exposure")

  props <- samplefiresarea %>%
    dplyr::select(c(.data$exposure, .data$proptotstudyarea,.data$proptotfiresarea,.data$propsampstudyarea,.data$propsampfiresarea))
  if (plot ==T){
    plt1 <- ggplot2::ggplot(props, ggplot2::aes(x = .data$exposure)) +
      ggplot2::geom_col(mapping = ggplot2::aes(y = .data$proptotfiresarea), fill = "grey") +
      ggplot2::geom_col(mapping = ggplot2::aes(y = .data$proptotstudyarea), fill =NA, col = 'black', linetype = 2)

    plt2 <- ggplot2::ggplot(props, ggplot2::aes(x = .data$exposure)) +
      ggplot2::geom_col(mapping = ggplot2::aes(y = .data$propsampfiresarea), fill = "grey") +
      ggplot2::geom_col(mapping = ggplot2::aes(y = .data$propsampstudyarea), fill =NA, col = 'black', linetype = 2)

    plts <- cowplot::plot_grid(plt1, plt2)
    return(plts)
  } else {
    return(props)
  }
}


