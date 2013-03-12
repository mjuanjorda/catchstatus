\name{get_froese_etal_2002_status}
\alias{get_froese_etal_2002_status}
\title{Return the Froese and Kesner-Reyes 2002 stock status based on a catch series.}
\description{Return the fisheries stock status from a catch series using the algorithm in:
@article{Froese:2002:IFA,
Author = {Rainer Froese and Kathleen Kesner-Reyes},
Journal = {ICES Council Meeting Report},
Pages = {1--16},
Title = {Impact of Fishing on the Abundance of Marine Species},
Volume = {CM 2002/L:12},
Year = {2002}}}
\usage{
get_froese_etal_2002_status(catch)
}
\arguments{
  \item{catch}{A time series of catch with no missing values.}
}

\value{A vector of statuses.}
\references{Froese, R. and Kesner-Reyes, K. 2002. Impact of fishing on the abundance of marine species. ICES Council Meeting Report, CM 2002/L:12: 1–16.
}
\author{Sean Anderson}



\seealso{\code{\link{plot_status_stacked}}, \code{\link{plot_status_ts}}, \code{\link{get_froese_etal_2012_status}}}
\examples{
set.seed(3)
x <- c(1:20, 20:1, rep(2, 5), 2:20, 19:1, rep(2, 10), 2:8)*10 + rnorm(100, 6, 8)
years <- 1901:2000
dat <- data.frame(year = years, catch = x)
plot(dat[,1:2], type = "o", pch = 20)
transform(dat, status = get_froese_etal_2002_status(catch))
}
