\name{plot_status_ts}
\alias{plot_status_ts}
\title{Plot a time series of catch with the status overlaid with coloured bars.}
\description{Takes a time series of catch and plots the series with colour bars
overlaid that indicate the assessed stock status.}
\usage{
plot_status_ts(catch,
year,
status,
xlab = "Year",
ylab = "Catch",
...)
}
\arguments{
  \item{catch}{The catch series.}
  \item{year}{The associated years to plot.}
  \item{status}{The status. Should be a factor with levels of 1:6 representing:
"Undeveloped", "Developing", "Rebuilding", "Fully exploited",
"Overexploited", and "Collapsed", in that order. }
  \item{xlab}{The x-axis label.}
  \item{ylab}{The y-axis label.}
  \item{\dots}{Other parameter values to pass to plot.default().}
}

\value{A plot of the time series with coloured status bars overlaid.}

\author{Sean Anderson}



\seealso{\code{\link{get_froese_etal_2012_status}}, \code{\link{plot_status_stacked}}}
\examples{
set.seed(3)
x <- c(1:20, 20:1, rep(2, 5), 2:20, 19:1, rep(2, 10), 2:8)*10 + rnorm(100, 6, 8)
years <- 1901:2000
dat <- data.frame(year = years, catch = x)
x <- transform(dat, status = get_froese_etal_2012_status(catch)$status)
with(x, plot_status_ts(catch, year, status))
}
