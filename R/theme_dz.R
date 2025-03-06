#' Ggplot theme Dezernat Zukunft
#'
#' @param base_size base size text
#'
#' @returns theme
#' @export
#'
#' @examples
#' \dontrun{
#' theme_dz()}
theme_dz<-function(base_size=base_size){
  theme_tufte(base_family = "Open Sans",base_size = base_size)+
    theme(panel.grid.major.y = element_line(linetype = "11",linewidth=0.5,color="#181c44"),
          axis.ticks.y = element_blank(),
          axis.ticks.x =element_line(color="#181c44"),
          legend.position = "bottom",
          text=element_text( color="#181c44"),
          axis.text=element_text( color="#181c44"),
          plot.title = element_text(margin = margin(b = 2)),
          plot.subtitle = element_text(margin = margin(t = 0)))
}
