# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Mar 19, 2012
# Last modified: Mar 28, 2012
# Purpose:       Make the typical stacked polygon status plot.
# ====================================================================

plot_status_stacked <- structure(function# Plot the status of fisheries through time using the typical stacked polygon layout.
### Takes the status of a group of fisheries through time and plots
### them in a stacked polygon layout. Data can be pre-aggregated by
### year or in long format (e.g. a column for stock ID, a column for year,
### and a column for status). The function will aggregate by year.
##
##seealso<< \code{\link{get_froese_etal_2012_status}}, \code{\link{plot_status_ts}}
(year, 
### The years to plot. 
 status, 
### The status. Should be a factor with levels of 1:6 representing:
 ### "Undeveloped", "Developing", "Rebuilding", "Fully exploited",
 ### "Overexploited", and "Collapsed", in that order. 
 xlab = "Year", 
### The x-axis label.
ylab = "Percentage of stocks", 
### The y-axis label.
mask_last_year = TRUE,
### Should the last year be plotted?
xlim = NULL,
### The x-axis limits. Defaults to the range of years given.
...
### Other parameter values to pass to plot.default().
) {
catch_status_df <- data.frame(year = year, status = status)
catch_status_df <- catch_status_df[order(catch_status_df$year), ]
require(plyr)
status_summary <- ddply(catch_status_df, "year", function(x) {
status_df <- as.data.frame(table(x$status) / sum(table(x$status)))
data.frame(Undeveloped = status_df[1,2], Developing = status_df[2,2], Rebuilding = status_df[3,2], Fully_exploited = status_df[4,2], Overexploited = status_df[5,2], Collapsed = status_df[6,2])
})

status_summary_cumsum <- transform(status_summary, Undeveloped = 1, Developing = Collapsed + Overexploited + Fully_exploited + Rebuilding + Developing, Rebuilding = Collapsed + Overexploited + Fully_exploited + Rebuilding, Fully_exploited = Collapsed + Overexploited + Fully_exploited, Overexploited = Collapsed + Overexploited, Collapsed = Collapsed, Bottom_of_plot = rep(0, length(Collapsed)))

status_labels <- c("Undeveloped", "Developing", "Rebuilding", "Fully exploited", "Overexploited", "Collapsed")
col_df <- data.frame(status = factor(1:6, levels = 1:6, labels = status_labels), col = c("#3AA2FE", "#22FF07", "#18A208", "#FFFF0B", "#FA0007", "#822B04"))
col_df$col <- as.character(col_df$col)

if(mask_last_year) status_summary_cumsum <- status_summary_cumsum[-nrow(status_summary_cumsum),]

if(is.null(xlim)) xlim <- range(status_summary_cumsum$year)
with(status_summary_cumsum, plot(1,1, xlim = xlim, ylim = c(0, 100), yaxs = "i", xaxs = "i", type = "n", xlab = xlab, ylab = ylab, ...))

for(i in 2:(ncol(status_summary_cumsum) - 1)) polygon(c(status_summary_cumsum$year,  rev(status_summary_cumsum$year)), c(status_summary_cumsum[,i]*100, rev(status_summary_cumsum[,i+1])*100), border = NA, col = col_df$col[i - 1])
invisible(list(status_summary_cumsum = status_summary_cumsum, status_summary = status_summary))
},ex=function(){
 data(srdb)
 library(plyr)
 out <- ddply(srdb, "assessid", transform, status = get_froese_etal_2012_status(catch)$status)
 with(out, plot_status_stacked(tsyear, status))
})


