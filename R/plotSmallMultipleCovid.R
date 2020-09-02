#' plotSmallMultipleCovid
#'
#' @param my_data
#' @param myvariable
#' @param colfeature
#' @param facetfeature
#' @param facetscales
#' @param ncol
#' @param mytitle_base
#'
#' @return
#' @export
#' @import grDevices
#' @import ggplot2
#'
#' @examples
plotSmallMultipleCovid <- function(
  my_data = NULL
  , myvariable = "death_cum"
  , colfeature = "region_name"
  , facetfeature = "dep_name"
  , facetscales = "free_y"
  , ncol = 10
  , mytitle_base = "France per departments : "
){

  # Define the number of colors you want
  nb_cols <- length(unique(my_data[[colfeature]]))
  mycolors <- grDevices::colorRampPalette(brewer.pal(8, "Dark2"))(nb_cols)

  my_data %>%
    ggplot2::ggplot(
      aes_string(
        x= "date"
        , y= myvariable
      )
    ) +
    ggplot2::geom_bar(
      stat="identity"
      , aes_string(fill= colfeature)
    ) +
    ggplot2::scale_fill_manual(values = mycolors) -> g

  if(!is.null(facetfeature)){
    if(is.null(facetscales)){
      g <- g + ggplot2::facet_wrap(
        as.formula(paste(". ~ ", facetfeature))
        , ncol = ncol
        #, scales = "free_y"
      )
    } else {
      g <- g + ggplot2::facet_wrap(
        as.formula(paste(". ~ ", facetfeature))
        , ncol = ncol
        , scales = facetscales
      )
    }
  }
  g +
    ggplot2::labs(
      y= "" #myvariable
      , title=paste0(mytitle_base, myvariable)
      #, subtitle="Note: differing y-axis scales"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      #legend.position = "none"
      strip.text.x = element_text(
        size=5
        , face = "bold"
        , vjust = -0.0,
        margin = margin(-0.00, 0, 0, 0, "cm")
      )
      , strip.background = element_rect(
        color ="white"
        , fill ="white"
        , size = 0.5
        #, linetype="solid"
      )
      , axis.text.x = element_text(
        angle = 90
        , size = 8
      )
      #, plot.background = element_rect(fill = '#effffe', colour = '#effffe')
    ) -> fr
  return(fr)
}
