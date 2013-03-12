\name{get_froese_etal_2012_status}
\alias{get_froese_etal_2012_status}
\title{Return the Froese et al. 2012 stock status based on a catch series.}
\description{Return the fisheries stock status from a catch series using the updated algorithm in:
@article{Froese:etal:2012,
Author = {Froese, Rainer and Zeller, Dirk and Kleisner, Kristin and Pauly, Daniel},
Journal = {Marine Biology},
Title = {What catch data can tell us about the status of global fisheries},
Volume = {In press},
Year = {2012}}}
\usage{
get_froese_etal_2012_status(catch)
}
\arguments{
  \item{catch}{A time series of catch with no missing values.}
}

\value{A list containing the named elements: ``status'', the stock
status; ``c_max_1.5'', whether the maximum was assumed to be 1.5
times the actual maximum catch; and ``fully_exploited_0.28'',
whether the stock was assumed to be rebuilt in the last year of
the series because the catch that year was 0.28 times or more of
the maximum catch.}
\references{Froese, R., Zeller, D., Kleisner, K., and Pauly, D. What catch data can tell us about the status of global fisheries? Mar. Biol. In press. doi: 10.1007/s00227-012-1909-6
}
\author{Sean Anderson}



\seealso{\code{\link{plot_status_stacked}}, \code{\link{plot_status_ts}}}
\examples{
set.seed(3)
x <- c(1:20, 20:1, rep(2, 5), 2:20, 19:1, rep(2, 10), 2:8)*10 + rnorm(100, 6, 8)
years <- 1901:2000
dat <- data.frame(year = years, catch = x)
plot(dat[,1:2], type = "o", pch = 20)
transform(dat, status = get_froese_etal_2012_status(catch)$status)
}
