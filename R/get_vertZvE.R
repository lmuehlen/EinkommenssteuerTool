#' Verteilung zu versteuerndes Einkommen approximieren
#'
#' Approximiert die Verteilung des zu versteuernden Einkommens mit Hilfe einer Spline Regression
#'
#' @param data_eink Tabelle 73111-02 aus Lohn- und Einkommenssteuerbericht hat Variablen "Zu_versteuerndes_Einkommen", "Grund-Splittingtabelle", "Steuerpflichtige_Anzahl", "Betrag_EUR". Kann weitere haben
#' @param nominallohn_fkt Faktor für Verschiebung der Nominallöhne e.g. Nominallohn_fkt=(1+"Annahme Lohnwachstum 2025")\*"Nominallohnindex Mai 2024"/"Nominallohnindex 2019"
#'
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @import magrittr
#' @importFrom stats spline
#' @importFrom fuzzyjoin fuzzy_left_join
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_extract
#'
#
#'
#' @returns dataframe inkl ZvE und Steuerpflichtige_norm_smooth
#' @export
#'
#' @examples
#' \dontrun{
#' get_vertZvE(data_eink_clean)}
get_vertZvE<-function(data_eink_clean){

data<-data_eink_clean
  ##### Spline Regression, um Verteilung zu erhalten, die exakt durch beobachtete Punkte geht.
  spline_result <- stats::spline(data$ZvE_Durchschnitt_adjusted, data$Steuerpflichtige_norm, n = 10,xout = 5000:max(data[data$ZvE_ub!=5000000,]$ZvE_Durchschnitt_adjusted),method = "fmm")

  #Lineare Interpolation zwischen der Steuerpflichtigen_norm der letzten gut definierten ZvE-Klasse und der höchsten Einkommensgruppe
  ## Verteilung bis ZvE von 1.000.000 (Kosten darüber können direkt berechnet werden)
  x_b<-data[data$ZvE_ub==1000000,]$ZvE_Durchschnitt_adjusted%>%as.integer()
  x_e<-data$ZvE_Durchschnitt_adjusted%>%max()
  y_b<-data[data$ZvE_ub==1000000,]$Steuerpflichtige_norm
  y_e<-data[data$ZvE_ub==5000000,]$Steuerpflichtige_norm

  x <- x_b:1000000
  y <- y_b + (y_e - y_b) * (x - x_b) / (x_e - x_b)
  df_linint <- data.frame(ZvE=x , Steuerpflichtige_norm_smooth = y)

  # join
  spline_data <- data.frame(ZvE = spline_result$x, Steuerpflichtige_norm_smooth = spline_result$y)%>%rbind(df_linint)


  eink_vert_gleichvert <- fuzzyjoin::fuzzy_left_join(
    spline_data,
    data,
    by = c(
      "ZvE" = "ZvE_lb",
      "ZvE" = "ZvE_ub"
    ),
    match_fun = list(`>=`, `<`)
  )

  return(eink_vert_gleichvert)
}
