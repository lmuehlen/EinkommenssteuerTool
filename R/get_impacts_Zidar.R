#' Impact Table Zidar
#'
#' Returns the impact effects on laborforce, hoursworked, consumption, GDP, and investment.
#' Based on the estimates of Zidar 2019
#'
#' @param df dataframe
#' @param GDP GDP at time of the reform
#'
#' @returns dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' get_impacts_Zidar(Reform2025_df)}
get_impacts_Zidar<-function(df,GDP=4406,long=F){
  Entlastung_TOT<-ZidarB90T10%>%filter(group=="Insgesamt")%>%pull(Entlastung_group)%>%round(2)
  Entlastung_T10<-ZidarB90T10%>%filter(group=="90-100")%>%pull(Entlastung_group)%>%round(2)
  Entlastung_B90<-Entlastung_TOT-Entlastung_T10%>%round(2)

  Entlastung_T10<-100*Entlastung_T10/GDP
  Entlastung_B90<-100*Entlastung_B90/GDP


  impacteff_laborforce_b90<-c(-1.6,-0.7,1.9,3.6,3.4) #pp
  impacteff_laborforce_t10<-c(-0.1,0.3,0.8,1,0.8) #pp
  impacteff_hoursworked_b90<-c(-0.4,-0.1,1.6,1.1,0.2) #perc
  impacteff_hoursworked_t10<-c(-0.4,-0.4,-0.3,-0.3,-0.2) #perc
  impacteff_consumption_b90<-c(2.1,4.2,4.1,2.5,-0.4) #perc
  impacteff_consumption_t10<-c(-0.6,-1.3,-0.8,-1,-2.2) #perc
  impacteff_GDP_b90<-c(-0.66,2.6,3.8,NA,NA) #perc
  impacteff_GDP_t10<-c(-1.78,0.4,1.1,NA,NA) #perc
  impacteff_investment_b90<-c(1.2,16.2,27.3,NA,NA) #perc
  impacteff_investment_t10<-c(-6.9,0.9,-0.4,NA,NA) #perc

  impact_laborforce<-Entlastung_B90*impacteff_laborforce_b90+Entlastung_T10*impacteff_laborforce_t10
  impact_hoursworked<-Entlastung_B90*impacteff_hoursworked_b90+Entlastung_T10*impacteff_hoursworked_t10
  impact_consumption<-Entlastung_B90*impacteff_consumption_b90+Entlastung_T10*impacteff_consumption_t10
  impact_GDP<-Entlastung_B90*impacteff_GDP_b90+Entlastung_T10*impacteff_GDP_t10
  impact_investment<-Entlastung_B90*impacteff_investment_b90+Entlastung_T10*impacteff_investment_t10

  table_impact<-tibble(
    Variable=c(
      rep("Labor force participation rate (pp)",5),
      rep("Hours worked (%)",5),
      rep("Consumption (%)",5),
      rep("GDP (%)",5),
      rep("Investment (%)",5)),
    time=rep(0:4,5),
    value=c(round(impact_laborforce,1),
            round(impact_hoursworked,1),
            round(impact_consumption,1),
            round(impact_GDP,1),
            round(impact_investment,1))
  )

  if(long){
    return(table_impact)
  }else{
    table_impact<-table_impact%>%
      pivot_wider(values_from = value,names_from = time,names_prefix = "Year ")
    return(table_impact)
  }

}
