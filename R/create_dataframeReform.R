#' Datensatz für Reformvorschlag
#'
#' @param data_eink Tabelle 73111-02 aus Lohn- und Einkommensteuerbericht
#' @param gfb_reform Grundfreibetrag Reform
#' @param pg1s_reform Startsatz erste Progressionszone
#' @param pg1e_reform Ende erste Progressionszone
#' @param pg2s_reform Startsatz zweite Progressionszone
#' @param pg2e_reform Ende zweite Progressionszone
#' @param pp1s_reform Satz erste Proportionalzone, Spitzensteuersatz
#' @param pp1e_reform Ende erste Proportionalzone
#' @param pp2s_reform Satz zweite Proportionalzone, Reichensteuersatz
#' @param solib_base  Solibeginn
#' @param solis_base  Solisatz
#' @param solig_base  Gleitfaktor Soli
#'
#' @param gfb_base Grundfreibetrag
#' @param pg1s_base Startsatz erste Progressionszone
#' @param pg1e_base Ende erste Progressionszone
#' @param pg2s_base Startsatz zweite Progressionszone
#' @param pg2e_base Ende zweite Progressionszone
#' @param pp1s_base Satz erste Proportionalzone, Spitzensteuersatz
#' @param pp1e_base Ende erste Proportionalzone
#' @param pp2s_base Satz zweite Proportionalzone, Reichensteuersatz
#' @param solib_reform Solibeginn
#' @param solis_reform Solisatz
#' @param solig_reform Gleitfaktor Soli
#'
#'
#' @returns dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' get_dataframeReform(data_eink)}
create_dataframeReform<-function(
    data_ZvE,data_vertZvE,
    gfb_reform=11604, pg1s_reform=14, pg1e_reform=17005, pg2s_reform=23.97, pg2e_reform=66760, pp1s_reform=42, pp1e_reform=277826, pp2s_reform=45,
    solib_base=18130, solis_base=5.5,solig_base=11.9,
    gfb_base=11604, pg1s_base=14, pg1e_base=17005, pg2s_base=23.97, pg2e_base=66760, pp1s_base=42, pp1e_base=277826, pp2s_base=45,
    solib_reform=18130, solis_reform=5.5,solig_reform=11.9){


  #data_ZvE<-clean_dataZvE(data_eink)
  #data_vertZvE<-get_vertZvE(data_ZvE)
  # Berechnungen für Personen mit zu versteuernden Einkommen über 1 Mio####
  #Anzahl und durchschnittliche zu versteuerndes Einkommen Steuerpflichtige mit zu versteuernden Einkommen über 1 Mio
  AnzahlSteuerpflichte_over1mio<-data_ZvE%>%filter(ZvE_lb==1000000)%>%pull(Steuerpflichtige_Gesamt)
  ZvEDurchschnitt_over1mio<-data_ZvE%>%filter(ZvE_lb==1000000)%>%pull(ZvE_Durchschnitt)
  #Durschnittliche zu zahlende Einkommensteuer für Steuerpflichtige mit zu versteuernden Einkommen über 1 Mio (Status quo und Reform)
 # Einkommensteuer_over1mio_base<-compute_Einkommensteuer(ZvEDurchschnitt_over1mio,gfb_base, pg1s_base, pg1e_base, pg2s_base, pg2e_base, pp1s_base, pp1e_base, pp2s_base)
  #Einkommensteuer_over1mio_reform<-compute_Einkommensteuer(ZvEDurchschnitt_over1mio,gfb_reform, pg1s_reform, pg1e_reform, pg2s_reform, pg2e_reform, pp1s_reform, pp1e_reform, pp2s_reform)
  #Steuereinnahmen durch Steuerpflichtige mit zu versteuernden Einkommen über 1 Mio
  #Einnahmen_over1mio_base<-AnzahlSteuerpflichte_over1mio*Einkommensteuer_over1mio_base
  #Einnahmen_over1mio_reform<-AnzahlSteuerpflichte_over1mio*Einkommensteuer_over1mio_reform
  #Mindereinnahmen_over1mio<-Einnahmen_over1mio_base-Einnahmen_over1mio_reform

  #Creating dataset####

  df<-data_vertZvE%>%
    add_row(ZvE=ZvEDurchschnitt_over1mio,Steuerpflichtige_norm_smooth=AnzahlSteuerpflichte_over1mio)%>%
    mutate(ZvE=ZvE,
           Steuerpflichtige_norm_smooth,
           Grenzsteuersatz_base=compute_Grenzsteuersatz(ZvE,gfb_base, pg1s_base, pg1e_base, pg2s_base, pg2e_base, pp1s_base, pp1e_base, pp2s_base),
           Einkommensteuer_base=compute_Einkommensteuer(ZvE,gfb_base, pg1s_base, pg1e_base, pg2s_base, pg2e_base, pp1s_base, pp1e_base, pp2s_base),
           Grenzsteuersatz_reform=compute_Grenzsteuersatz(ZvE,gfb_reform, pg1s_reform, pg1e_reform, pg2s_reform, pg2e_reform, pp1s_reform, pp1e_reform, pp2s_reform),
           Einkommensteuer_reform=compute_Einkommensteuer(ZvE,gfb_reform, pg1s_reform, pg1e_reform, pg2s_reform, pg2e_reform, pp1s_reform, pp1e_reform, pp2s_reform),
           Durchschnittssteuersatz_base=100*Einkommensteuer_base/ZvE,
           Einnahmen_base=Einkommensteuer_base*Steuerpflichtige_norm_smooth,
           Durchschnittssteuersatz_reform=100*Einkommensteuer_reform/ZvE,
           Einnahmen_reform=Einkommensteuer_reform*Steuerpflichtige_norm_smooth,
           Entlastung_pcZvE=Durchschnittssteuersatz_base-Durchschnittssteuersatz_reform,
           Entlastung_absolut=Einkommensteuer_base-Einkommensteuer_reform,
           Mindereinnahmen=Einnahmen_base-Einnahmen_reform
    )%>%
    mutate(
      #SoliGrenzsteuersatz
      Soli_Grenzsteuersatz_base=case_when(
        is.na(Einkommensteuer_base)~NA_real_,
        Einkommensteuer_base<solib_base~0,
        (Einkommensteuer_base-solib_base)*solig_base/100<Einkommensteuer_base*solis_base/100~solig_base*Grenzsteuersatz_base/100,
        TRUE~solis_base*Grenzsteuersatz_base/100
      ),
      Soli_Grenzsteuersatz_reform=case_when(
        is.na(Einkommensteuer_reform)~NA_real_,
        Einkommensteuer_reform<solib_reform~0,
        (Einkommensteuer_reform-solib_reform)*solig_reform/100<Einkommensteuer_reform*solis_reform/100~solig_reform*Grenzsteuersatz_reform/100,
        TRUE~solis_reform*Grenzsteuersatz_reform/100
      ),
      Grenzsteuersatz_inklSoli_base=Grenzsteuersatz_base+Soli_Grenzsteuersatz_base,
      Grenzsteuersatz_inklSoli_reform=Grenzsteuersatz_reform+Soli_Grenzsteuersatz_reform,
      #Solitotal
      Solitotal_base=case_when(
        is.na(Soli_Grenzsteuersatz_base)~NA_real_,
        Soli_Grenzsteuersatz_base==0~0,
        Soli_Grenzsteuersatz_base==solig_base*Grenzsteuersatz_base/100~(Einkommensteuer_base-solib_base)*solig_base/100,
        Soli_Grenzsteuersatz_base==solis_base*Grenzsteuersatz_base/100~Einkommensteuer_base*solis_base/100
      ),
      Solitotal_reform=case_when(
        is.na(Soli_Grenzsteuersatz_reform)~NA_real_,
        Soli_Grenzsteuersatz_reform==0~0,
        Soli_Grenzsteuersatz_reform==solig_reform*Grenzsteuersatz_reform/100~(Einkommensteuer_reform-solib_reform)*solig_reform/100,
        Soli_Grenzsteuersatz_reform==solis_reform*Grenzsteuersatz_reform/100~Einkommensteuer_reform*solis_reform/100
      ),
      Einkommensteuer_inklSoli_base=Einkommensteuer_base+Solitotal_base,
      Einkommensteuer_inklSoli_reform=Einkommensteuer_reform+Solitotal_reform,
      Durchschnittssteuersatz_inklSoli_base=100*Einkommensteuer_inklSoli_base/ZvE,
      Einnahmen_inklSoli_base=Einkommensteuer_inklSoli_base*Steuerpflichtige_norm_smooth,
      Durchschnittssteuersatz_inklSoli_reform=100*Einkommensteuer_inklSoli_reform/ZvE,
      Einnahmen_inklSoli_reform=Einkommensteuer_inklSoli_reform*Steuerpflichtige_norm_smooth,
      Entlastung_inklSoli_pcZvE=Durchschnittssteuersatz_inklSoli_base-Durchschnittssteuersatz_inklSoli_reform,
      Entlastung_inklSoli_absolut=Einkommensteuer_inklSoli_base-Einkommensteuer_inklSoli_reform,
      Mindereinnahmen_inklSoli=Einnahmen_inklSoli_base-Einnahmen_inklSoli_reform,
      AnteilZvE=cumsum(Steuerpflichtige_norm_smooth)/sum(Steuerpflichtige_norm_smooth,na.rm = T)
    )%>%
    mutate(across(matches("Grenz|Durchschnitt"),~ replace(., . == 0, NA)))%>%
    select(-c(ZvE_lb,ZvE_ub,ZvE_Durchschnitt,ZvE_Durchschnitt_adjusted,Steuerpflichtige_norm))
  return(df)
}
