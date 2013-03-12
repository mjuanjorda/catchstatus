\name{plot_status_stacked}
\alias{plot_status_stacked}
\title{Plot the status of fisheries through time using the typical stacked polygon layout.}
\description{Takes the status of a group of fisheries through time and plots
them in a stacked polygon layout. Data can be pre-aggregated by
year or in long format (e.g. a column for stock ID, a column for year,
and a column for status). The function will aggregate by year.}
\usage{
plot_status_stacked(year,
status,
xlab = "Year",
ylab = "Percentage of stocks",
mask_last_year = TRUE,
xlim = NULL,
...)
}
\arguments{
  \item{year}{The years to plot. }
  \item{status}{The status. Should be a factor with levels of 1:6 representing:
"Undeveloped", "Developing", "Rebuilding", "Fully exploited",
"Overexploited", and "Collapsed", in that order. }
  \item{xlab}{The x-axis label.}
  \item{ylab}{The y-axis label.}
  \item{mask_last_year}{Should the last year be plotted?}
  \item{xlim}{The x-axis limits. Defaults to the range of years given.}
  \item{\dots}{Other parameter values to pass to plot.default().}
}



\author{Sean Anderson}



\seealso{\code{\link{get_froese_etal_2012_status}}, \code{\link{plot_status_ts}}}
\examples{
data(srdb)
library(plyr)
out <- ddply(srdb, "assessid", transform, status = get_froese_etal_2012_status(catch)$status)
with(out, plot_status_stacked(tsyear, status))
}
