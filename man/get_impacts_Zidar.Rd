% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_impacts_Zidar.R
\name{get_impacts_Zidar}
\alias{get_impacts_Zidar}
\title{Impact Table Zidar}
\usage{
get_impacts_Zidar(df, GDP = 4406, long = F)
}
\arguments{
\item{df}{dataframe}

\item{GDP}{GDP at time of the reform}
}
\value{
dataframe
}
\description{
Returns the impact effects on laborforce, hoursworked, consumption, GDP, and investment.
Based on the estimates of Zidar 2019
}
\examples{
\dontrun{
get_impacts_Zidar(Reform2025_df)}
}
