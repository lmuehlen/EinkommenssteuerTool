#' Get Entlastungen
#'
#'Entlastungen (Kosten) insgesamt und nach n-til (im Status Quo der Steuerpflichtigen)
#'
#' @param data Dataframe Reform
#' @param n_groups Anzahl Gruppen
#' @param formated Soll ein rein numerische Datensatz oder ein bereits formatierter exportiert werden?
#'
#' @returns dataframe or formated dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' get_dataframe_Entlastungen(SPD_2025,10)
#' }
gen_dataframe_Entlastungen <- function(data, n_groups = 10,formated=FALSE) {


#   Entlastung insgesamt (Gesamtkosten)
  Entlastung_gesamt <- data$Mindereinnahmen_inklSoli %>%
    sum(na.rm = TRUE) / 1e9

  # Entlastung aufgeteilt nach n-til der im Status Quo Steuerpflichtigen
  Entlastung_dataframe <- data %>%
    filter(ZvE > 12096) %>%
    arrange(ZvE) %>%
    mutate(
      # AnteilZvE neu berechnen, da hier nur diejenigen gewollt die zuvor Steuern gezahlt haben
      AnteilZvE = cumsum(Steuerpflichtige_norm_smooth) /
        sum(Steuerpflichtige_norm_smooth, na.rm = TRUE),

      # AnteilZvE aufteilen in n Gruppen
      group = cut(
        AnteilZvE,
        breaks = seq(0, 1, length.out = n_groups + 1),
        # Labels
        labels = paste0(
          seq(0, 100 - 100/n_groups, by = 100/n_groups),
          "-",
          seq(100/n_groups, 100, by = 100/n_groups)
        ),
        include.lowest = TRUE,
        right = FALSE
      )
    ) %>%
    group_by(group) %>%
    summarise(
      Entlastung_group = sum(Mindereinnahmen_inklSoli, na.rm = TRUE) / 1e9,
      Entlastung_group_pc = 100 * Entlastung_group / Entlastung_gesamt,

      # Weighted mean innerhalb von Gruppe für individuelle Entlastung
      Entlastung_ind = weighted.mean(
        x = Entlastung_inklSoli_absolut,
        w = Steuerpflichtige_norm_smooth / sum(Steuerpflichtige_norm_smooth, na.rm = TRUE),
        na.rm = TRUE
      ),
      Entlastung_ind_pcZvE = weighted.mean(
        x = Entlastung_inklSoli_pcZvE,
        w = Steuerpflichtige_norm_smooth / sum(Steuerpflichtige_norm_smooth, na.rm = TRUE),
        na.rm = TRUE
      ),
      .groups = "drop"
    )%>%
    ungroup()%>%
    add_row(group="Insgesamt",Entlastung_group=sum(.$Entlastung_group),Entlastung_group_pc=sum(.$Entlastung_group_pc),Entlastung_ind=mean(.$Entlastung_ind),Entlastung_ind_pcZvE=mean(.$Entlastung_ind_pcZvE))


  Entlastung_table<-Entlastung_dataframe%>%
    reframe(
      "Name"=group,
      "Entlastung/Kosten (Mrd)"=Entlastung_group%>%round(.,1)%>%paste0(" Mrd. €"),
      "Entlastung (% Gesamt)"=Entlastung_group_pc%>%round(.,1)%>%paste0(" %"),
      "Entlastung Individuum"=Entlastung_ind%>%round(.,1)%>%paste0(" €"),
      "Entlastung Individuum (% ZvE)"=Entlastung_ind_pcZvE%>%round(.,1)%>%paste0(" %")
    )

  #list_entlastung<-list(
  #  Entlastung_gesamt = Entlastung_gesamt,
  #  Entlastung_Übersicht = Entlastung_Uebersicht
  #)

if(formated){
  return(Entlastung_table)
}else{
  return(Entlastung_dataframe)
}
}
