#' Wachstumseffekte
#'
#' @param data Reformvorschlag
#' @param multiplikator Impulse Response für Tax Shock in Höhe 1% GDP
#' @param GDP Aktuelles GDP
#' @param steuerelast Steuerelastizität (Default 0.97 basierend auf )
#' @param steuerschätzung Steuerschätzung Default von
#' @param inflationsan Angenommene Inflation für die nächsten Jahre (für Annahme, dass sich die Kosten entsprechend der Inflation entwickeln)
#'
#' @importFrom purrr map_dbl
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#'
#' @returns dataframe
#' @export
#'
#' @examples
#' compute_wachstumseffekte(10)
compute_wachstumseffekte<-function(data,impulse_response=c(0.25,1.1,2.35,2.4),Jahre=2025:2028,GDP=4406,steuerelast=0.97,steuerschätzung=c(982.4,1024.9,1063.2,1133.8),inflationsan=2){


Kosten<-compute_Gesamtkosten(data)

  effektgdp<-(GDP+Kosten/GDP*impulse_response)

  steuermehreinnahmen<-effektgdp*steuerelast*steuerschätzung

  kosten_nominal<-purrr::map_dbl(0:3,~Kosten*(1+inflationsan/100)^(.x)) #Annahme Kosten entwickeln sich entsprechend der Inflation
  finanzierungslücke<-kosten_nominal-steuermehreinnahmen

  list(
    Jahr=Jahre,
    "Kosten in Mrd."=kosten_nominal,
    "Zusätzliches Wachstum in %"=100*effektgdp,
    "Steuermehreinnahmen in Mrd."=steuermehreinnahmen,
    "Finanzierungslücke in Mrd."=finanzierungslücke
  )%>%
    as_tibble()%>%
    tidyr::pivot_longer(-Jahr,values_to = "values",names_to = "names")%>%
    tidyr::pivot_wider(values_from = values,names_from = "Jahr")
}
