multidirexp <- function(expl, values, plot = F, all = F){
  fts <- values

  df <- data.frame()

  for (i in 1:length(fts)) {
    dat <- direxp(expl, fts[i], table = T) %>%
      dplyr::select(-.data$wkt) %>%
      dplyr::mutate(featureID = i) %>%
      tidyr::pivot_wider(names_from = .data$seg, values_from = .data$viable) %>%
      dplyr::select(.data$featureID, tidyselect::everything())
    df <- rbind(df, dat)
  }

  df2 <- df %>%
    dplyr::mutate(full = ifelse(.data$to5+.data$to10+.data$to15==3,1,0)) %>%
    dplyr::mutate(outer = ifelse(.data$to10+.data$to15==2,1,0))

  if (plot == T){
    dfsums <- df2 %>%
      dplyr::group_by(.data$deg) %>%
      dplyr::summarise(freq = ifelse(all==T,sum(.data$full),sum(.data$outer)))
    plt <- ggplot2::ggplot(dfsums, ggplot2::aes(.data$deg, .data$freq)) +
      ggplot2::geom_bar(stat = "identity",
                        width = 1,
                        fill = "yellow",
                        color = "yellow") +
      ggplot2::coord_polar() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background =  ggplot2::element_rect(fill = 'transparent'),
        plot.background = ggplot2::element_rect(fill = 'transparent', color =
                                                  NA),
        panel.border = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(
          color = "black",
          size = 15,
          face = "bold"
        ),
        axis.ticks.y = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank()
      )  +
      ggplot2::scale_x_continuous(breaks = c(90, 180, 270, 360),
                                  labels = c("E", "S", "W", "N")) +
      ggplot2::labs(title = "Directional Exposure Load for Multiple Values",
                    subtitle = "Plot generated with fireexposuR()")
    return(plt)
  } else{
    return(df2)
  }
}
