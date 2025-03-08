% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_vertZvE.R
\name{get_vertZvE}
\alias{get_vertZvE}
\title{Verteilung zu versteuerndes Einkommen approximieren}
\usage{
get_vertZvE(data_eink_clean)
}
\arguments{
\item{data_eink}{Tabelle 73111-02 aus Lohn- und Einkommenssteuerbericht hat Variablen "Zu_versteuerndes_Einkommen", "Grund-Splittingtabelle", "Steuerpflichtige_Anzahl", "Betrag_EUR". Kann weitere haben}

\item{nominallohn_fkt}{Faktor für Verschiebung der Nominallöhne e.g. Nominallohn_fkt=(1+"Annahme Lohnwachstum 2025")\*"Nominallohnindex Mai 2024"/"Nominallohnindex 2019"}
}
\value{
dataframe inkl ZvE und Steuerpflichtige_norm_smooth
}
\description{
Approximiert die Verteilung des zu versteuernden Einkommens mit Hilfe einer Spline Regression
}
\examples{
\dontrun{
get_vertZvE(data_eink_clean)}
}
