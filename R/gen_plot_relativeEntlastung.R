#' Abbildung relative Entlastung
#'
#' @param df dataframe
#' @param breaks breaks x-Axis
#' @param limit limit x-Axis
#' @param limitneg_y lower bound y-Axis
#' @param limitpos_y upper bound y-Axis
#'
#' @returns ggplot
#' @export
#'
#' @examples
#' \dontrun{
#' gen_plot(relativeEntlastung(SPD2025_df))}
#'

gen_plot_relativeEntlastung<-function(df=NULL,breaks=c(10000,30000,100000,500000),limit=500000,limitneg_y=NULL,limitpos_y=NULL){

  vgl_df<-df%>%filter(ZvE<=limit)

  labels=formatC(breaks,big.mark = ".",decimal.mark = ",", format = "f", digits = 0)

  # Ober und Untergrenze Plot ermitteln (auf bzw abgerundet auf 2er Grenze)
  if(is.null(limitneg_y)){
    limitneg_y<--2*ceiling(min(vgl_df[vgl_df$ZvE<=limit,]$Entlastung_inklSoli_pcZvE,na.rm=T)/(-2))
  }

  if(is.null(limitpos_y)){
    limitpos_y<-2*ceiling(max(vgl_df[vgl_df$ZvE<=limit,]$Entlastung_inklSoli_pcZvE,na.rm=T)/2)
  }

  # farbiger Hintergrund

  y_breaks<-seq(limitneg_y,limitpos_y,by=2)

  df_bands <- data.frame(
    y_min = c(seq(limitneg_y, 0, by = 1),seq(0, limitpos_y-1, by = 1)),
    y_max = c(seq(limitneg_y+1, 0,  by = 1),seq(0,limitpos_y,by=1))
  )
  df_bands$band_id <- seq_len(nrow(df_bands))
  my_colors<-colorRampPalette(c("#ee6174","grey95","#57C773"))(nrow(df_bands))

  # Tatsächliche Grafik####

  vis_ent_pc<-vgl_df%>%
    ggplot(aes(x=log(ZvE)))+

    #farbiger Hintergrund
    geom_rect(
      data = df_bands,
      aes(
        xmin = log(10000),
        xmax =  log(500000),
        ymin = y_min,
        ymax = y_max,
        fill = factor(band_id)
      ),
      color = NA,
      inherit.aes = FALSE
    ) +
    scale_fill_manual(
      values = my_colors,
      guide = "none"
    ) +
    coord_cartesian(clip = "off")+
    #Line plot
     geom_line(aes(y=Entlastung_inklSoli_pcZvE),color="#181c44")+

    #Design
    scale_x_continuous(limits = c(log(10000),log(limit)),expand=c(0.014,0.014),breaks = log(breaks),labels = labels)+
    scale_y_continuous(limits = c(limitneg_y,limitpos_y),breaks=y_breaks)+
    labs(x="Zu versteuerndes Einkommen (ZvE)",y=NULL,title="Entlastung",subtitle = " in % des zu versteuernden Einkommens")+
    theme_dz(base_size = 10)+
    theme(panel.grid.major.y = element_blank())

  return(vis_ent_pc)
}
