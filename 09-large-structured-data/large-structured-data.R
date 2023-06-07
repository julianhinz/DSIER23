### set wd to source file dir
setwd("~/work/Teaching/DSIER23/09-large-structured-data")

# https://dsier.julianhinz.com/03-large-data/Expo_2019.zip

if (!require("pacman")) install.packages("pacman"); library(pacman)
if (!require("poweRlaw")) install.packages("poweRlaw"); library(poweRlaw)

# Load the libraries
pacman::p_load(tidyverse)
pacman::p_load(data.table)


# define a function that select only the csv files within a zip folder
find_csv_name <- function(zipfile) {
  # Create a name for the dir where we'll unzip
  zipdir <- tempfile()
  # Create the dir using that name
  dir.create(zipdir)
  # Unzip the file into the dir
  unzip(zipfile, exdir=zipdir)
  # Get a list of csv files in the dir
  files <- list.files(zipdir)
  files <- files[grep("\\.csv$", files)]
  print(files)
  return(files)}

# define a function that 
# 1. pull all monthly data together, specify same format of input variables (the name of the company, the export value)
# 2. group by company and get the firm level export value as a sum of the export by firm across product-destinations
# Load the parallel package
library(parallel)

# Detect the number of cores
num_cores <- detectCores()

# Print the number of cores
print(num_cores)
pull_months <- function(dt) {
  print(paste0("Working on ", dt, ""))
  data = vroom::vroom(unzip(dt, find_csv_name(dt)),  col_select = c(NIT, FOBDOL), col_types = c(NIT = "c", FOBDOL = "d"))
  file.remove(find_csv_name(dt))
  return(data)
}


# create a vector that contain the list of name files 
my_files <- list.files("input/Expo_2019", full.names = TRUE)

# create a vector that contain the list of name files 
dat <- map_df(my_files, pull_months)

setDT(dat)
# create a data
data_plot = dat[!is.na(FOBDOL), .(value = sum(FOBDOL)), by = NIT]

# plot size histogram
plot = ggplot(data = data_plot) +
  theme_minimal() +
  geom_histogram(aes(x = value), bins = 40) +
  geom_vline(aes(xintercept = data_plot[, mean(value)]), color = "red") +
  geom_vline(aes(xintercept = data_plot[, median(value)]), color  = "blue") +
  scale_x_log10(name = "Value in USD") +
  scale_y_continuous(name = "Number of firms")
ggsave(plot, filename = "output/historgram_firmsize.png", width = 20, height = 10, units = "cm")

# Statistical tests: evaluate the goodness of fit for a log-normal distribution. 
# One commonly used test is the Shapiro-Wilk test, which tests the null hypothesis 
# that the data follows a normal distribution. Since if Y has a normal distribution, 
# then the exponential function of Y, X = exp(Y), has a log-normal distribution., a normality test  can indirectly indicate 
# the log-normality. 
set.seed(1)
shapiro.test(log(sample(data_plot$value, 500,replace = FALSE, prob = NULL)))

qqnorm(log(data_plot$value))
qqline(log(data_plot$value))

# Notice the points fall along a line in the middle of the graph, but curve off in the extremities. 
# Normal QQ plots that exhibit this behavior usually mean your data have more extreme values than 
# would be expected if they truly came from a normal distribution. 

qqplot(qnorm(ppoints(100)), qcauchy(ppoints(100)))

# plot size distribution ---- 

# Create a continuous power law object
m_pl <- conpl$new(data_plot$value)

m_pl = conpl$new(data_plot$value)
est = estimate_xmin(m_pl)

est = estimate_xmin(m_pl,  xmax = 1e+06)
m_pl$setXmin(est)

xmin = est$xmin  # minimum parameter
alpha = est$pars # alpha parameter (slope)

# Here, plot(m_pl) will create a log-log plot of the empirical complementary cumulative distribution function (CCDF) of your data. 
# lines(m_pl, col = "red") will add a line to this plot showing the fitted power law model.

plot(m_pl, main = "Power Law Fit", xlab = "Total Export (log)", ylab = "P(X >= x)")
lines(m_pl, col = "red")



# Subset the data based on xmin
data_subset <- data_plot$value[data_plot$value >= est$xmin]

# Create a new power law object with the subset data
m_pl_subset <- conpl$new(data_subset)
est_subset = estimate_xmin(m_pl_subset, xmax = 1e+06)
m_pl_subset$setXmin(est_subset)

# Plot the empirical data and the fitted model
plot(m_pl_subset, main = "Power Law Fit", xlab = "x", ylab = "P(X >= x)")
lines(m_pl_subset, col = "red")

# # Load the parallel package
# library(parallel)
# # Detect the number of cores
# num_cores <- detectCores()
# # Print the number of cores
# print(num_cores)

# Perform the goodness-of-fit test, using 
bs = bootstrap_p(m_pl, threads = 8, no_of_sims= 100, xmax = 1e+06 )

# bootstrap_p
# Resampling: The function resamples the data with replacement. This means it creates a new dataset of the 
# same size by randomly drawing data points from the original dataset, allowing for the same data point to be selected more than once.
# # Parameter Estimation: For each resampled dataset, the function estimates the power law parameters 
# (e.g., the scale parameter xmin and the exponent alpha).
# # Comparison: The function then compares these estimates to the parameters estimated from the original dataset. 
# Specifically, it computes the Kolmogorov-Smirnov statistic, a measure of the difference between the empirical distribution function 
# of the sample and the cumulative distribution function of the reference distribution, for each resampled dataset.
# # P-Value Computation: Finally, the function computes a p-value based on the fraction of the resampled datasets 
# for which the Kolmogorov-Smirnov statistic is greater than the statistic computed from the original dataset. 
# A larger p-value (e.g., p > 0.1) indicates that the power law model is a plausible fit to the data.


# Print the p-value
# If the p-value > 0.1, we can't reject the hypothesis that the data follows a power law
print(bs$p)


# Create an exponential object
m_exp = conexp$new(data_plot$value)
est_exp = estimate_xmin(m_exp, xmax=1e+06)
m_exp$setXmin(est_exp)

# Create a log-normal object
m_ln = lognormal$new(data)
est_ln = estimate_xmin(m_ln)
m_ln$setXmin(est_ln)


# Compare power law with exponential
ll_pl = logLik(m_pl)
ll_exp = logLik(m_exp)
print(ll_pl - ll_exp)

# Compare power law with log-normal
ll_ln = logLik(m_ln)
print(ll_pl - ll_ln)


# Plot the empirical data and the fitted models
plot(m_pl, main = "Data with Fitted Models", xlab = "x", ylab = "P(X >= x)")
lines(m_pl, col = "red", lwd = 2)
lines(m_exp, col = "blue", lwd = 2)
lines(m_ln, col = "green", lwd = 2)

# Add a legend
legend("bottomleft", legend = c("Power Law", "Exponential", "Log-normal"), 
       col = c("red", "blue", "green"), lwd = 2)



