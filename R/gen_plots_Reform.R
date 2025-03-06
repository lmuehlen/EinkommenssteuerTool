#' Gemeinsamer Plot Reform Entlastung
#'
#' @param df Dataframe Reform
#' @param breaks_gstdst breaks für Plot Grenz- und Durchschnittssteuersatz
#' @param max_x_gstdst limit x-Axis Plot Grenz- und Durchschnittssteuersatz
#' @param max_y_gstdst limit y-Axis Plot Grenz- und Durchschnittssteuersatz
#' @param max_x_ent limit x-Axis Plots Entlastung
#' @param max_y_ent limit y-Axis Plots Entlastung
#' @param max_val Breite Gruppe Entlastung
#' @param fast fast computation (select only every 200th "observation")
#'
#' @returns ggplots
#' @export
#'
#' @examples
#' \dontrun{
#' get_plots_Reform(SPD2025_df)
#' }
gen_plots_Reform<-function(df,breaks_gstdst=c(0,30000,60000,90000,120000),breaks_ent=c(10000,30000,100000,500000),max_x_gstdst=125000,max_y_gstdst=60,max_x_ent=500000,max_y_ent=NULL,max_val=2,fast=FALSE){
  vis_gstdst<- gen_plot_GrenzDurchschnittssteuersatz(df,breaks=breaks_gstdst,max_x_gstdst,max_y_gstdst,fast=fast)
  vis_entpc<-gen_plot_relativeEntlastung(df,breaks=breaks_ent,max_x=max_x_ent,max_y=max_y_ent,fast=fast)
  vis_einkvert<-gen_plot_EntlastungEinkommensverteilung(df,breaks=breaks_ent,max_x=max_x_ent,width=width_ent,max_val=max_val)
  p<-wrap_plots(vis_gstdst,vis_entpc,vis_einkvert,nrow = 1)

  return(p)
}
