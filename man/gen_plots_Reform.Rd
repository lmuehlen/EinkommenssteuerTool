% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_plots_Reform.R
\name{gen_plots_Reform}
\alias{gen_plots_Reform}
\title{Gemeinsamer Plot Reform Entlastung}
\usage{
gen_plots_Reform(
  df,
  breaks_gstdst = c(0, 30000, 60000, 90000, 120000),
  breaks_ent = c(10000, 30000, 1e+05, 5e+05),
  max_x_gstdst = 125000,
  max_y_gstdst = 60,
  max_x_ent = 5e+05,
  max_y_ent = NULL,
  width_ent = 2,
  max_val = 8,
  fast = FALSE
)
}
\arguments{
\item{df}{Dataframe Reform}

\item{breaks_gstdst}{breaks für Plot Grenz- und Durchschnittssteuersatz}

\item{max_x_gstdst}{limit x-Axis Plot Grenz- und Durchschnittssteuersatz}

\item{max_y_gstdst}{limit y-Axis Plot Grenz- und Durchschnittssteuersatz}

\item{max_x_ent}{limit x-Axis Plots Entlastung}

\item{max_y_ent}{limit y-Axis Plots Entlastung}

\item{max_val}{Breite Gruppe Entlastung}

\item{fast}{fast computation (select only every 200th "observation")}
}
\value{
ggplots
}
\description{
Gemeinsamer Plot Reform Entlastung
}
\examples{
\dontrun{
get_plots_Reform(SPD2025_df)
}
}
