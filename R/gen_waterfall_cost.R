#' Wasserfall plot
#'
#' @param data_Wachstumseffekte Dataframe Wachstumseffekte
#' @param year Jahr (muss enthalten sein in )
#' @param max_y Obergrenze y-Achse
#' @param title Titel
#' @param subtitle Untertitel (e.g. Einheit)
#'
#' @returns ggplot
#' @export
#'
#' @examples
#' \dontrun{
#' Wasserfall_plot(Reform2025_df)}
gen_plot_WaterfallCost<-function(data,Jahre=c("2025","2026","2027","2028"),max_y=NULL,title=NULL,subtitle=NULL){
  #  year<-"2025"
   data<-Reform2025_df
  # nolegend=T
  # max_y=120
#data_Wachstumseffekte<-compute_Wachstumseffekte(Reform2025_df)
#Jahre=c("2025","2026","2027","2028")



data_Wachstumseffekte<-compute_Wachstumseffekte(data)

if(is.null(max_y)){
  max_y<-1.15*compute_Gesamtkosten(data)
}

data_WaterfallPlot<-purrr::map(Jahre,function(year){
 gen_dataframe_Waterfallplot(data_Wachstumseffekte,year=year,max_y=max_y)%>%
   mutate(year=year)
}
)%>%do.call("rbind",.)

  #tatsächlichen Plot erstellen####
  #plot<-
plot<-data_WaterfallPlot%>%
    ggplot(aes(cat,values,fill=type))+
    geom_hline(yintercept = 0)+
    geom_col(aes(alpha = type))+
    geom_label(aes(y=label_height,label = label),show.legend = FALSE,color="#181c44",fill="white",family="Open Sans",size=2.5)+
    facet_wrap(~year,nrow=1,ncol=4)+
    #Design
    labs(x=NULL,y=NULL,title=title,subtitle=subtitle,color=NULL,fill=NULL,alpha=NULL)+
    scale_y_continuous(limits = c(0,max_y))+
    scale_fill_manual(values=c("Steuermehreinnahmen"="#57C773",NA,"Kommunen"="#646dc8","Länder"="#a55fbd","Bund"="#ee6174"),
                      breaks = c("Kommunen", "Länder", "Bund"))+
    scale_alpha_manual(values = c(1,0,1,1,1))+
    guides(alpha=guide_none())+
    guides(fill=guide_none())+
    theme_dz(base_size=10)

#plot
  return(plot)

}




#' Internal Function for WaterfallPlot
#'
#' @param data_Wachstumseffekte dataframe Wachstumseffekte
#' @param year Jahr des Wachstumseffekts
#' @param max_y Obergrenze Y-Achse
#'
#' @returns dataframe
#'
#' @examples
#' \dontrun{gen_dataframe_Waterfallplot(data_Wachstumseffekte)}
gen_dataframe_Waterfallplot<-function(data_Wachstumseffekte,year="2025",max_y=NULL){

#Datensatz einlesen und bereinigen
  data<-data_Wachstumseffekte%>%
    dplyr::filter(names!="Zusätzliches Wachstum in %")%>%
    dplyr::select(all_of(year))%>%
    dplyr::mutate(names=c("Kosten","Mehreinnahmen","Finanzierungslücke"))%>%
    tidyr::pivot_wider(values_from = year,names_from = names)


  # Datensatz in Bund Länder und Kommunen, Steuermehreinnahmen aufteilen####
  #Label generieren
  data_waterfallPlot<-tibble(
    cat=c(rep("Kosten",5),rep("Mehreinnahmen\n Wachstum",5),rep("Lücke",5))%>%
      factor(levels = c("Kosten","Mehreinnahmen\n Wachstum","Lücke")),
    type=rep(c("Bund","Länder","Kommunen","transp","Steuermehreinnahmen"),3)%>%
      factor(levels = rev(c("Bund","Länder","Kommunen","transp","Steuermehreinnahmen"))),
    values=c(0.425*data$Kosten,0.425*data$Kosten,0.15*data$Kosten,0,0,
             0,0,0,data$Finanzierungslücke,data$Mehreinnahmen,
             0.425*data$Finanzierungslücke,0.425*data$Finanzierungslücke,0.15*data$Finanzierungslücke,0,0),
    label_height=c(NA,NA,NA,data$Kosten+max_y*0.042,0,
                   0,0,0,data$Kosten+max_y*0.042,0,
                   NA,NA,NA,data$Finanzierungslücke+max_y*0.042,0
    ),
    label=c(NA,NA,NA,round(data$Kosten,1),NA,
            NA,NA,NA,round(data$Mehreinnahmen,1),NA,
            NA,NA,NA,round(data$Finanzierungslücke,1),NA
    )
  )

  return(data_waterfallPlot)
}
