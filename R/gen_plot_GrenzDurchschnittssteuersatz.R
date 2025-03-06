#' Plot Grenz- und Durchschnittssteuersatz
#'
#' @param df dataframe des Reformvorschlags
#' @param max_x Beschränkung X-Achse (ZvE)
#' @param max_y Beschränkung Y-Achse (Steuersatz)
#' @param breaks
#'
#' @importFrom dplyr pull
#' @import ggplot2
#'
#'
#'
#'
#'
#' @returns plotgen
#' @export
#'
#' @examples
#' \dontrun{gen_plot_GrenzDurchschnittssteuersatz(SPD2025_df)}
gen_plot_GrenzDurchschnittssteuersatz<-function(df=NULL,max_x=125000,max_y=60,fast=FALSE,breaks=c(0,30000,60000,90000,120000)){

  df<-df%>%filter(ZvE<=max_x)

  if(fast){
  df<-df%>%filter(ZvE%%200==0)
  }

  labels=formatC(breaks,big.mark = ".",decimal.mark = ",", format = "f", digits = 0)
  #Grenzen Plots

  if(is.null(max_y)){
    max_y<-df%>%pull(Grenzsteuersatz_inklSoli_reform)%>%max(na.rm = T)/10%>%ceiling()*10
    #gstdst_ub<-10*ceiling(max(df$Grenzsteuersatz_inklSoli_reform,na.rm=T)/10)
  }

  vis_gst_dst<-df%>%
    ggplot(aes(x=ZvE))+
    geom_hline(yintercept = 0)+
    #Grenzsteuersatz
    geom_line(aes(y=Grenzsteuersatz_inklSoli_base,color="Status quo"))+
    geom_line(aes(y=Grenzsteuersatz_inklSoli_reform,color="Reform"))+
    #Durchschnittssteuersatz
    geom_line(aes(y=Durchschnittssteuersatz_inklSoli_base,color="Status quo"),linetype="dashed")+
    geom_line(aes(y=Durchschnittssteuersatz_inklSoli_reform,color="Reform"),linetype="dashed")+
    #Design
    scale_x_continuous(limits = c(0,max_x),expand = c(0.01,0.01),breaks=breaks,labels = labels)+
    scale_y_continuous(limits = c(0,max_y))+
    scale_color_manual(
      values = c("Reform" = "#EE6174", "Status quo" = "#181c44")
    )+
    guides(color=guide_legend(position="inside"))+
    labs(x=NULL,y=NULL,title="Grenz- und Durchschnittssteuersatz",subtitle = "in Prozent")+
    theme_dz(base_size = 10)+
    theme(
      legend.position.inside = c(0.84, 0.19),  # Legende in die untere linke Ecke verschieben
      legend.background = element_rect(fill = "transparent",color="transparent"),  # Hintergrund halbtransparent
      legend.title = element_blank() )
  return(vis_gst_dst)
}
