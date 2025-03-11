
#' Compute Gesamtkosten
#'
#' @param data Dataframe Reform
#'
#' @returns number
#' @export
#'
#' @examples
#' \dontrun{
#' compute_Gesamtkosten(Reform2025_df)
#' }
compute_Gesamtkosten<-function(data){
  Entlastung_gesamt <- data$Mindereinnahmen_inklSoli %>%
    sum(na.rm = TRUE) / 1e9

  return(Entlastung_gesamt)
}
