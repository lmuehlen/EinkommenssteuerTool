#' Title
#'
#' @param gfb_vgl
#' @param pg1s_vgl
#' @param pg1e_vgl
#' @param pg2s_vgl
#' @param pg2e_vgl
#' @param pp1s_vgl
#' @param pp1e_vgl
#' @param pp2s_vgl
#' @param solib_base
#' @param solis_base
#' @param solig_base
#' @param gfb_base
#' @param pg1s_base
#' @param pg1e_base
#' @param pg2s_base
#' @param pg2e_base
#' @param pp1s_base
#' @param pp1e_base
#' @param pp2s_base
#' @param solib_vgl
#' @param solis_vgl
#' @param solig_vgl
#' @param eink_file
#'
#' @returns
#' @export
#'
#' @examples
create_dataframeReform<-function(
    eink_vert,
    gfb_vgl=11604, pg1s_vgl=14, pg1e_vgl=17005, pg2s_vgl=23.97, pg2e_vgl=66760, pp1s_vgl=42, pp1e_vgl=277826, pp2s_vgl=45,
    solib_base=18130, solis_base=5.5,solig_base=11.9,
    gfb_base=11604, pg1s_base=14, pg1e_base=17005, pg2s_base=23.97, pg2e_base=66760, pp1s_base=42, pp1e_base=277826, pp2s_base=45,
    solib_vgl=18130, solis_vgl=5.5,solig_vgl=11.9){


  data_ZvE<-clean_dataZvE(data_eink)
  data_vertZvE<-get_vertZvE(dataZvE)
  # Berechnungen für Personen mit zu versteuernden Einkommen über 1 Mio####
  #Anzahl und durchschnittliche zu versteuerndes Einkommen Steuerpflichtige mit zu versteuernden Einkommen über 1 Mio
  AnzahlSteuerpflichte_over1mio<-data_ZvE%>%filter(ZvE_lb==1000000)%>%pull(Steuerpflichtige_Gesamt)
  ZvEDurchschnitt_over1mio<-data_ZvE%>%filter(ZvE_lb==1000000)%>%pull(ZvE_Durchschnitt)
  #Durschnittliche zu zahlende Einkommenssteuer für Steuerpflichtige mit zu versteuernden Einkommen über 1 Mio (Status quo und Reform)
  Einkommenssteuer_over1mio_base<-compute_Einkommensteuer(ZvEDurchschnitt_over1mio,gfb_base, pg1s_base, pg1e_base, pg2s_base, pg2e_base, pp1s_base, pp1e_base, pp2s_base)
  Einkommenssteuer_over1mio_vgl<-compute_Einkommensteuer(ZvEDurchschnitt_over1mio,gfb_vgl, pg1s_vgl, pg1e_vgl, pg2s_vgl, pg2e_vgl, pp1s_vgl, pp1e_vgl, pp2s_vgl)
  #Steuereinnahmen durch Steuerpflichtige mit zu versteuernden Einkommen über 1 Mio
  Einnahmen_over1mio_base<-AnzahlSteuerpflichte_over1mio*Einkommenssteuer_over1mio_base
  Einnahmen_over1mio_vgl<-AnzahlSteuerpflichte_over1mio*Einkommenssteuer_over1mio_vgl
  Mindereinnahmen_over1mio<-Einnahmen_over1mio_base-Einnahmen_over1mio_vgl

  #Creating dataset####
  eink_vert_gleichvert<-readRDS(paste0("dfs/",eink_file))
  df<-eink_vert_gleichvert%>%
    mutate(ZvE=ZvE, #Anpassung!!!
           Steuerpflichtige_norm_smooth,
           Grenzsteuersatz_base=compute_Grenzsteuersatz(ZvE,gfb_base, pg1s_base, pg1e_base, pg2s_base, pg2e_base, pp1s_base, pp1e_base, pp2s_base),
           Einkommenssteuer_base=compute_Einkommensteuer(ZvE,gfb_base, pg1s_base, pg1e_base, pg2s_base, pg2e_base, pp1s_base, pp1e_base, pp2s_base),
           Grenzsteuersatz_vgl=compute_Grenzsteuersatz(ZvE,gfb_vgl, pg1s_vgl, pg1e_vgl, pg2s_vgl, pg2e_vgl, pp1s_vgl, pp1e_vgl, pp2s_vgl),
           Einkommenssteuer_vgl=compute_Einkommenssteuer(ZvE,gfb_vgl, pg1s_vgl, pg1e_vgl, pg2s_vgl, pg2e_vgl, pp1s_vgl, pp1e_vgl, pp2s_vgl),
           Durchschnittssteuersatz_base=100*Einkommenssteuer_base/ZvE,
           Einnahmen_base=Einkommenssteuer_base*Steuerpflichtige_norm_smooth,
           Durchschnittssteuersatz_vgl=100*Einkommenssteuer_vgl/ZvE,
           Einnahmen_vgl=Einkommenssteuer_vgl*Steuerpflichtige_norm_smooth,
           Entlastung_pcZvE=Durchschnittssteuersatz_base-Durchschnittssteuersatz_vgl,
           Entlastung_absolut=Einkommenssteuer_base-Einkommenssteuer_vgl,
           Mindereinnahmen=Einnahmen_base-Einnahmen_vgl
    )%>%
    add_row(Mindereinnahmen=Mindereinnahmen_over1mio)%>%
    mutate(
      #SoliGrenzsteuersatz
      Soli_Grenzsteuersatz_base=case_when(
        is.na(Einkommenssteuer_base)~NA_real_,
        Einkommenssteuer_base<solib_base~0,
        (Einkommenssteuer_base-solib_base)*solig_base/100<Einkommenssteuer_base*solis_base/100~solig_base*Grenzsteuersatz_base/100,
        TRUE~solis_base*Grenzsteuersatz_base/100
      ),
      Soli_Grenzsteuersatz_vgl=case_when(
        is.na(Einkommenssteuer_vgl)~NA_real_,
        Einkommenssteuer_vgl<solib_vgl~0,
        (Einkommenssteuer_vgl-solib_vgl)*solig_vgl/100<Einkommenssteuer_vgl*solis_vgl/100~solig_vgl*Grenzsteuersatz_vgl/100,
        TRUE~solis_vgl*Grenzsteuersatz_vgl/100
      ),
      Grenzsteuersatz_inklSoli_base=Grenzsteuersatz_base+Soli_Grenzsteuersatz_base,
      Grenzsteuersatz_inklSoli_vgl=Grenzsteuersatz_vgl+Soli_Grenzsteuersatz_vgl,
      #Solitotal
      Solitotal_base=case_when(
        is.na(Soli_Grenzsteuersatz_base)~NA_real_,
        Soli_Grenzsteuersatz_base==0~0,
        Soli_Grenzsteuersatz_base==solig_base*Grenzsteuersatz_base/100~(Einkommenssteuer_base-solib_base)*solig_base/100,
        Soli_Grenzsteuersatz_base==solis_base*Grenzsteuersatz_base/100~Einkommenssteuer_base*solis_base/100
      ),
      Solitotal_vgl=case_when(
        is.na(Soli_Grenzsteuersatz_vgl)~NA_real_,
        Soli_Grenzsteuersatz_vgl==0~0,
        Soli_Grenzsteuersatz_vgl==solig_vgl*Grenzsteuersatz_vgl/100~(Einkommenssteuer_vgl-solib_vgl)*solig_vgl/100,
        Soli_Grenzsteuersatz_vgl==solis_vgl*Grenzsteuersatz_vgl/100~Einkommenssteuer_vgl*solis_vgl/100
      ),
      Einkommenssteuer_inklSoli_base=Einkommenssteuer_base+Solitotal_base,
      Einkommenssteuer_inklSoli_vgl=Einkommenssteuer_vgl+Solitotal_vgl,
      Durchschnittssteuersatz_inklSoli_base=100*Einkommenssteuer_inklSoli_base/ZvE,
      Einnahmen_inklSoli_base=Einkommenssteuer_inklSoli_base*Steuerpflichtige_norm_smooth,
      Durchschnittssteuersatz_inklSoli_vgl=100*Einkommenssteuer_inklSoli_vgl/ZvE,
      Einnahmen_inklSoli_vgl=Einkommenssteuer_inklSoli_vgl*Steuerpflichtige_norm_smooth,
      Entlastung_inklSoli_pcZvE=Durchschnittssteuersatz_inklSoli_base-Durchschnittssteuersatz_inklSoli_vgl,
      Entlastung_inklSoli_absolut=Einkommenssteuer_inklSoli_base-Einkommenssteuer_inklSoli_vgl,
      Mindereinnahmen_inklSoli=Einnahmen_inklSoli_base-Einnahmen_inklSoli_vgl,
      AnteilZvE=cumsum(Steuerpflichtige_norm_smooth)/sum(Steuerpflichtige_norm_smooth,na.rm = T)
    )%>%
    mutate(across(matches("Grenz|Durchschnitt"),~ replace(., . == 0, NA)))%>%
    select(-c(ZvE_lb,ZvE_ub,ZvE_Durchschnitt,ZvE_Durchschnitt_adjusted,Steuerpflichtige_norm))
  return(df)
}
