# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Mar 19, 2012
# Last modified: Mar 19, 2012
# Purpose:       Take a series of statuses and add them to a time
# series plot as a series of coloured bars.
# ====================================================================

plot_status_ts <- structure(function# Plot a time series of catch with the status overlaid with coloured bars.
### Takes a time series of catch and plots the series with colour bars
### overlaid that indicate the assessed stock status.
##
##seealso<< \code{\link{get_froese_etal_2012_status}}, \code{\link{plot_status_stacked}}
(catch, 
### The catch series.
 year, 
### The associated years to plot.
 status, 
### The status. Should be a factor with levels of 1:6 representing:
 ### "Undeveloped", "Developing", "Rebuilding", "Fully exploited",
 ### "Overexploited", and "Collapsed", in that order. 
 xlab = "Year", 
### The x-axis label.
 ylab = "Catch",
### The y-axis label.
 ...
### Other parameter values to pass to plot.default().
) {
x <- data.frame(year = year, catch = catch, status = status)
status_labels <- c("Undeveloped", "Developing", "Rebuilding", "Fully exploited", "Overexploited", "Collapsed")
col_df <- data.frame(status = factor(1:6, levels = 1:6, labels = status_labels), col = c("#3AA2FE", "#22FF07", "#18A208", "#FFFF0B", "#FA0007", "#822B04"))
col_df$col <- as.character(col_df$col)
x <- merge(x, col_df)
x <- x[order(x$year), ]
n <- length(x$year)

with(x, plot(year, catch, pch = 20, cex = 0.8, ylim = c(0, max(catch) * 1.05), yaxs = "i", xlab = xlab, ylab = ylab, type = "n", xaxs = "i", ...))
with(x, rect(year[1:(n-1)], 0, year[2:n], max(catch * 1.05), col = col, border = NA))
with(x, lines(year, catch))
with(x, points(year, catch, pch = 20, cex = 0.7, col = "grey20"))
### A plot of the time series with coloured status bars overlaid.
}
,ex=function(){
set.seed(3)
x <- c(1:20, 20:1, rep(2, 5), 2:20, 19:1, rep(2, 10), 2:8)*10 + rnorm(100, 6, 8)
years <- 1901:2000
dat <- data.frame(year = years, catch = x)
x <- transform(dat, status = get_froese_etal_2012_status(catch)$status)
with(x, plot_status_ts(catch, year, status))
})


