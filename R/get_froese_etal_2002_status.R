# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Mar 18, 2012
# Last modified: Mar 29, 2012
# Purpose:       Get the stock status as defined by 
# the original algorithm in Froese and Kesner-Reyes 2002
# ====================================================================

get_froese_etal_2002_status <- structure(function# Return the Froese and Kesner-Reyes 2002 stock status based on a catch series.
### Return the fisheries stock status from a catch series using the algorithm in:
### @article{Froese:2002:IFA,
###	 Author = {Rainer Froese and Kathleen Kesner-Reyes},
###	 Journal = {ICES Council Meeting Report},
###	 Pages = {1--16},
###	 Title = {Impact of Fishing on the Abundance of Marine Species},
###	 Volume = {CM 2002/L:12},
###	 Year = {2002}}
(catch
### A time series of catch with no missing values.
##
##references<<Froese, R. and Kesner-Reyes, K. 2002. Impact of fishing on the abundance of marine species. ICES Council Meeting Report, CM 2002/L:12: 1–16.
##
##seealso<< \code{\link{plot_status_stacked}}, \code{\link{plot_status_ts}}, \code{\link{get_froese_etal_2012_status}}
) {
  x <- catch
status_labels <- c("Undeveloped", "Developing", "Rebuilding", "Fully exploited", "Overexploited", "Collapsed")
  years <- 1:length(catch) # fake years to work with
d <- data.frame(year = years, catch = catch, status = factor(NA, levels = 1:6, labels = status_labels))

c_max <- max(x)
year_c_max <- years[x == c_max][1]
year_0.5_c_max <- years[x >= 0.5 * c_max][1]

# undeveloped
d[years < year_c_max & x < 0.1 * c_max, "status"] <- factor(1, levels = 1:6, labels = status_labels)

# developing
d[years < year_c_max & x >= 0.1 * c_max & x <= 0.5 * c_max, "status"] <- factor(2, levels = 1:6, labels = status_labels) 

# fully exploited
d[x > 0.5 * c_max, "status"] <- factor(4, levels = 1:6, labels = status_labels) 

# overexploited
d[years >= year_c_max & x <= 0.5 * c_max & x >= 0.1 * c_max, "status"] <- factor(5, levels = 1:6, labels = status_labels)

# collapsed
d[years >= year_c_max & x < 0.1 * c_max, "status"] <- factor(6, levels = 1:6, labels = status_labels) 

# if all 0s then set to undeveloped:
if(length(unique(d$catch)) == 1 & unique(d$catch)[1] %in% 0) d$status <- factor(1, levels = 1:6, labels = status_labels)

return(d$status)
### A vector of statuses.
}
,ex=function(){
set.seed(3)
x <- c(1:20, 20:1, rep(2, 5), 2:20, 19:1, rep(2, 10), 2:8)*10 + rnorm(100, 6, 8)
years <- 1901:2000
dat <- data.frame(year = years, catch = x)
plot(dat[,1:2], type = "o", pch = 20)
transform(dat, status = get_froese_etal_2002_status(catch))
}
)

