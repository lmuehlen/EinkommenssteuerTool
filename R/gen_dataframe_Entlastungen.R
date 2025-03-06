#' Get Entlastungen
#'
#'Entlastungen (Kosten) insgesamt und nach n-til (im Status Quo der Steuerpflichtigen)
#'
#' @param data Dataframe Reform
#' @param n_groups Anzahl Gruppen
#'
#' @returns list
#' @export
#'
#' @examples
#' \dontrun{
#' get_dataframe_Entlastungen(SPD_2025,10)
#' }
gen_dataframe_Entlastungen <- function(data, n_groups = 10) {

  # Entlastung insgesamt (Gesamtkosten)
  Entlastung_gesamt <- data$Mindereinnahmen_inklSoli %>%
    sum(na.rm = TRUE) / 1e9

  # Entlastung aufgeteilt nach n-til der im Status Quo Steuerpflichtigen
  Entlastung_Uebersicht <- data %>%
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
    )

  list_entlastung<-list(
    Entlastung_gesamt = Entlastung_gesamt,
    Entlastung_Übersicht = Entlastung_Uebersicht
  )

  return(list_entlastung)
}
