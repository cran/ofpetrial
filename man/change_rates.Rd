% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/change_rates.R
\name{change_rates}
\alias{change_rates}
\title{Change the assigned rates}
\usage{
change_rates(
  td,
  input_name = NA,
  strip_ids,
  plot_ids = NULL,
  new_rates,
  rate_by = "all"
)
}
\arguments{
\item{td}{trial design}

\item{input_name}{(character) input name}

\item{strip_ids}{(numeric) vector of strip_ids}

\item{plot_ids}{(numeric) vector of plot_ids}

\item{new_rates}{(numeric) single numeric number for `rate_by = "all"``, a vector of numeric values for `rate_by = "strip"``, a matrix of numeric numbers for `rate_by = "plot"`.}

\item{rate_by}{(character) default is "all". The other options are "plot" and "strip".}
}
\value{
trial design with changed rates
}
\description{
Change the assigned rates by plot and strip
}
\examples{
#--- load rate information ---#
data(td_single_input)

#--- change rates of some strips ---#
strip_ids <- 1:5
plot_ids <- 5:10
new_rates <- 200

td_modified <- change_rates(td_single_input, "NH3", strip_ids, plot_ids, new_rates)

#--- visualize ---#
viz(td_modified)
}
