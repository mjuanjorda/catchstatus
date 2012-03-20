# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Mar 18, 2012
# Last modified: Mar 19, 2012
# Purpose:       Get the stock status as defined by the updated
# algorithm in Froese et al. 2012. Mar. Biol.
# ====================================================================

get_froese_etal_2012_status <- structure(function# Return the Froese et al. 2012 stock status based on a catch series.
### Return the fisheries stock status from a catch series using the updated algorithm in:
### @article{Froese:etal:2012,
### 	Author = {Froese, Rainer and Zeller, Dirk and Kleisner, Kristin and Pauly, Daniel},
### 	Journal = {Marine Biology},
### 	Title = {What catch data can tell us about the status of global fisheries},
### 	Volume = {In press},
### 	Year = {2012}}
(catch
### A time series of catch with no missing values.
##
##references<<Froese, R., Zeller, D., Kleisner, K., and Pauly, D. What catch data can tell us about the status of global fisheries? Mar. Biol. In press. doi: 10.1007/s00227-012-1909-6
##
##seealso<< \code{\link{plot_status_stacked}}, \code{\link{plot_status_ts}}
) {
  x <- catch
status_labels <- c("Undeveloped", "Developing", "Rebuilding", "Fully exploited", "Overexploited", "Collapsed")
  years <- 1:length(catch) # fake years to work with
d <- data.frame(year = years, catch = catch, status = factor(NA, levels = 1:6, labels = status_labels))

c_max <- max(x)
year_c_max <- years[x == c_max][1]
year_0.5_c_max <- years[x >= 0.5 * c_max][1]

# Check if Cmax occurs in final year. If it does then increase Cmax by
# 50% and make it happen one year after the last year.
# We will remove this fake year at the end before returning the
# results.
if(year_c_max == max(years)) {
    c_max_1.5 <- TRUE
    c_max <- c_max * 1.5
    d <- rbind(d, data.frame(year = 9999, catch = c_max, status = factor(NA, levels = 1:6, labels = status_labels)))
  } else {
    c_max_1.5 <- FALSE
}

# undeveloped
d[years < year_0.5_c_max & x < 0.1 * c_max, "status"] <- factor(1, levels = 1:6, labels = status_labels)

# developing
d[years < year_0.5_c_max & x >= 0.1 * c_max & x <= 0.5 * c_max, "status"] <- factor(2, levels = 1:6, labels = status_labels) 

# fully exploited
d[years >= year_0.5_c_max & x > 0.5 * c_max, "status"] <- factor(4, levels = 1:6, labels = status_labels) 

# overexploited
d[years >= year_0.5_c_max & x <= 0.5 * c_max & x >= 0.1 * c_max, "status"] <- factor(5, levels = 1:6, labels = status_labels)

# collapsed
d[years >= year_0.5_c_max & x < 0.1 * c_max, "status"] <- factor(6, levels = 1:6, labels = status_labels) 

# figure out how many collapsed followed by recoveries there are:
# check all collapses to see if a full exploited comes after:
  #browser()
rows_with_collapsed <- (1:nrow(d))[d$status == "Collapsed"]
rows_where_collapse_begins <- rows_with_collapsed[diff(c(-99, rows_with_collapsed)) != 1]

# if the last year is a collapse then we aren't going to deal with it,
# because it can't recover by definition, so remove it here from the
# vector:
rows_where_collapse_begins <- rows_where_collapse_begins[!rows_where_collapse_begins %in% nrow(d)]

n_collapses <- length(rows_where_collapse_begins)

# so we can check the last one: (unless the last one is the start of a
# collapse)
#if(max(rows_where_collapse_begins) != nrow(d))
rows_where_collapse_begins <- c(rows_where_collapse_begins, (nrow(d)+1))

# check if should stay collapsed to end of time series:
# if there is never a fully exploited after than the whole series
# should stay collapsed from there on:

# In the final year, accept C[0.28 C/Cmax as indicative of subsequent fully exploited status
# So, make the extra year at the end have a status of Fully exploited
# if this extra year already exists than fill that status
# otherwise create an extra year and give it that status while
# assigning it a catch equal to the last year's catch value

fully_exploited_0.28 <- FALSE # default value

if(catch[length(years)] >= 0.28 * c_max & c_max_1.5 == TRUE) {
  fully_exploited_0.28 <- TRUE
  d[nrow(d), "status"] <- factor(4, levels = 1:6, labels = status_labels)                                             
}
if(catch[length(years)] >= 0.28 * c_max & c_max_1.5 == FALSE) {
  fully_exploited_0.28 <- TRUE
  d <- rbind(d, data.frame(year = 9999, catch = catch[length(years)], status = factor(4, levels = 1:6, labels = status_labels)))
} 

d$adjusted_status <- d$status
if(n_collapses > 0) {
for(i in 1:n_collapses) {
if(!"Fully exploited" %in% d[(rows_where_collapse_begins[i]+1):nrow(d), "status"]) { # never recovers from here on, so stay collapsed:
  d[(rows_where_collapse_begins[i] + 1):nrow(d), "adjusted_status"] <- factor(6, levels = 1:6, labels = status_labels) # these are now collapsed
} else { # there is at least one recovery, so rebuild until the next fully exploited:
  data_collapse_to_fully_exploited <- d[(rows_where_collapse_begins[i]+1):nrow(d), ]
  row_with_first_fully_exploited <- (1:nrow(data_collapse_to_fully_exploited))[data_collapse_to_fully_exploited$status == "Fully exploited"][1]
    d[(rows_where_collapse_begins[i] + 1):(rows_where_collapse_begins[i] + row_with_first_fully_exploited - 1), "adjusted_status"] <- factor(3, levels = 1:6, labels = status_labels) # these are now rebuilding
}
}
}

# if we added a 1.5Cmax fake year at the end, remove it before giving
# back results:
if(c_max_1.5 | fully_exploited_0.28) d <- d[-nrow(d),]
return(list(status = d$adjusted_status, c_max_1.5 = c_max_1.5, fully_exploited_0.28 = fully_exploited_0.28))
### A list containing the named elements: ``status'', the stock
### status; ``c_max_1.5'', whether the maximum was assumed to be 1.5
### times the actual maximum catch; and ``fully_exploited_0.28'',
### whether the stock was assumed to be rebuilt in the last year of
### the series because the catch that year was 0.28 times or more of
### the maximum catch.
}
,ex=function(){
set.seed(3)
x <- c(1:20, 20:1, rep(2, 5), 2:20, 19:1, rep(2, 10), 2:8)*10 + rnorm(100, 6, 8)
years <- 1901:2000
dat <- data.frame(year = years, catch = x)
plot(dat[,1:2], type = "o", pch = 20)
transform(dat, status = get_froese_etal_2012_status(catch)$status)
}
)

