
#' get_vertZvE
#'
#' @param data_eink Tabelle 73111-02 aus Lohn- und Einkommenssteuerbericht hat Variablen "Zu_versteuerndes_Einkommen", "Grund-Splittingtabelle", "Steuerpflichtige_Anzahl", "Betrag_EUR". Kann weitere haben
#' @param nominallohn_fkt Faktor für Verschiebung der Nominallöhne e.g. Nominallohn_fkt=(1+"Annahme Lohnwachstum 2025")\*"Nominallohnindex Mai 2024"/"Nominallohnindex 2019"
#'
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom tidyr pivot_wider
#' @importFrom stats spline
#' @importFrom fuzzyjoin fuzzy_left_join
#' @importFrom magrittr %>%
#'
#'
#'
#'
#' @returns dataframe inkl ZvE und Steuerpflichtige_norm_smooth
#' @export
#'
#' @examples
#' \dontrun{
#' get_vertZvE(data_eink,1)}
get_vertZvE<-function(data_eink,nominallohn_fkt=1){

  data<-data_eink%>%
    dplyr::filter(Merkmal_1=="Zu versteuerndes Einkommen",Zu_versteuerndes_Einkommen!="Insgesamt")%>%

    #Grenzen von Text auslesen
    # Splitting und Einzelveranlagung zusammenziehen (Splitting Grenzen halbieren , um zu matchen)
    dplyr::select(ZvE=Zu_versteuerndes_Einkommen,Einzel_Split=Grund_Splittingtabelle,Steuerpflichtige=Steuerpflichtige_Anzahl,Betrag=Betrag_EUR)%>%
    mutate(
      Einzel_Split=case_when(grepl("Grundtabelle",Einzel_Split)~"Einzel",
                             grepl("Splittingtabelle",Einzel_Split)~"Splitting"),
      ZvE_lb = as.numeric(str_remove_all(str_extract(ZvE, "(?<=von )\\d+(?: \\d+)*"), " ")),
      ZvE_lb = dplyr::case_when(Einzel_Split=="Splitting"~ZvE_lb/2,
                         TRUE~ZvE_lb),
      ZvE_lb = dplyr::case_when(grepl("unter 0",ZvE)~0,
                         grepl("oder mehr",ZvE)~1000000,
                         TRUE~ZvE_lb),
      ZvE_ub = as.numeric(str_remove_all(str_extract(ZvE, "(?<=unter )\\d+(?: \\d+)*"), " ")),
      ZvE_ub = dplyr::case_when(Einzel_Split=="Splitting"~ZvE_ub/2,
                         TRUE~ZvE_ub),
      ZvE_ub = dplyr::case_when (is.na(ZvE_ub)~5000000,
                          TRUE~ZvE_ub) #brauch ich nur für Interpolation (Kosten werden direkt exakt berechnet)
    )%>%
    dplyr::select(-ZvE)%>%
    dplyr::filter(ZvE_lb>=2500)%>%# untersten Einkommen interessiert uns nicht und verzerrt Verteilung (anderer DGP wegen Minijobs etc.)
    tidyr::pivot_wider(names_from = "Einzel_Split",values_from = c("Steuerpflichtige","Betrag"))%>%

    #Erstellung neue Variablen
    dplyr::mutate(
      Steuerpflichtige_Splitting=Steuerpflichtige_Splitting*2, #Splitting: Paar wird als ein gemeinsamer Haushalt gezählt (Mal 2, um mit Einzelveranlagung mergen zu können)
      #Gesamt Zahlen
      Steuerpflichtige_Gesamt=Steuerpflichtige_Einzel+Steuerpflichtige_Splitting,
      Betrag_Gesamt=Betrag_Einzel+Betrag_Splitting,
      #Durchschnitt ZvE in jeweiliger Klasse
      ZvE_Durchschnitt=Betrag_Gesamt/Steuerpflichtige_Gesamt,
      #ZvE_Durchschnitt fortgeschrieben gemäß Steigerung Nominallohn
      ZvE_Durchschnitt_adjusted=ZvE_Durchschnitt*nominallohn_fkt,

      #normierte Anzahl Steuerpflichte pro Einheit ZvE in Klasse (auf breite der Klasse anpassen)
      Steuerpflichtige_norm=Steuerpflichtige_Gesamt/(ZvE_ub-ZvE_lb),

    )%>%
    dplyr::select(ZvE_lb,ZvE_ub,ZvE_Durchschnitt,ZvE_Durchschnitt_adjusted,Steuerpflichtige_norm)%>%
    dplyr::filter(ZvE_ub>5000|ZvE_lb==1000000)


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
