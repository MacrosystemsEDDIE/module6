# Title: R code version of Macrosystems EDDIE Module 6: Understanding Uncertainty in Ecological Forecasts
# Author: Mary Lofton
# Date: 08NOV22

# This R script contains code to reproduce the basic functionality of Module 6 outside of R Shiny.
# The code can be used by students to better understand what is happening "under the hood"
# of the Shiny app.

# We will install and load some packages that are needed to run the script
# install.packages("tidyverse")
# install.packages("ncdf4")
# install.packages("lubridate")
# install.packages("RColorBrewer")
# install.packages("reshape")
# install.packages("ggthemes")
library(tidyverse)
library(ncdf4)
library(lubridate)
library(RColorBrewer)
library(reshape)
library(ggthemes)

# REMEMBER TO SET YOUR WORKING DIRECTORY!!
setwd("./app/mod6_R") #fill in your working directory here and make sure all data files and folders are in it

# 1. Select a NEON lake site ----

# For now, we are going to use Lake Barco, FL, USA as our example in this R script.
lake <- "BARC"

# 2. Retrieve the air temperature and water temperature data for Lake Barco and plot it ----

# Read in air temperature data for Lake Barco
# We are calling this "xvar" because we are using it as a predictor of water temperature
xvar <- read.csv("./BARC_airt_celsius.csv")

# Data wrangling to reformat dates 
xvar$Date <- as.Date(xvar[, 1])

# Data wrangling to calculate daily average air temperature
xvar <- plyr::ddply(xvar, c("Date"), function(x) mean(x[, 2], na.rm = TRUE)) 

# Read in surface water temperature data
# We are calling this "yvar" because it is the variable we are trying to predict
yvar <- read.csv("./BARC_wtemp_celsius.csv")

# Data wrangling to reformat dates
yvar$Date <- as.Date(yvar[, 1])

# Data wrangling to subset water temperature to only surface temperature and calculate daily average
yvar <- yvar[yvar[, 2] == min(yvar[, 2], na.rm = TRUE), c(1, 3)] # subset to Surface water temperature
yvar <- plyr::ddply(yvar, c("Date"), function(y) mean(y[, 2], na.rm = TRUE)) # Daily average 

# Combine air temperature and surface water temperature into one dataframe
lake_df <- merge(xvar, yvar, by = "Date")

# Subset to the months of May-October (when data is available for most NEON lakes)
lake_df$month <- lubridate::month(lake_df$Date)
lake_df <- lake_df[(lake_df$month %in% 5:10), 1:3]

# Rename columnns to sensible names after joining
colnames(lake_df)[-1] <- c("airt", "wtemp")

# Limit data to complete cases (rows with both air and water temperature available)
lake_df$airt[is.na(lake_df$wtemp)] <- NA
lake_df$wtemp[is.na(lake_df$airt)] <- NA

# Set custom color palette for our plot - ooh we are fancy!! :-)
cols <- RColorBrewer::brewer.pal(8, "Dark2")

# Build time series plot of air temperature and water temperature
p1 <- ggplot() +
  geom_line(data = lake_df, aes(Date, airt, color = "Air temperature")) +
  geom_line(data = lake_df, aes(Date, wtemp, color = "Water temperature")) +
  scale_color_manual(values = cols[5:6], name = "") +
  ylab("Temperature (\u00B0C)") +
  xlab("Time") +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  theme_bw(base_size = 18)

# Render plot - this should match the time series plot you see in the Shiny app in 
# Activity A, Objective 3: Build a water temperature model IF you selected Lake Barco as your site
p1

#3. Retrieve the air temperature forecast for Lake Barco from NOAA and plot it ----

# Name our forecast date: we will be working with a NOAA forecast generated on 2020-09-25
fc_date = "2020-09-25"

# Name the file path where we can find the NOAA forecast 
fpath <- file.path("./NOAA_FC_BARC")

# Name the forecast variables we want to retrieve: in our case, we are interested in air temperature
fc_vars <- "air_temperature"

# NOTE: NOAA forecasts are provided in a type of file called a netcdf file. In brief, netcdfs are data
# frames that have multiple dimensions (more than the two dimensions represented by rows and 
# columns in a typical spreadsheet). This netcdf file format requires some wrangling in order to
# pull it into R and format it as a two-dimensional data frame. Here, lines XX-XX sequentially open
# several netcdf files which contain NOAA forecast data and combine the information we need from 
# each file so that we end up with a two-dimensional data frame containing a 7-day-ahead 
# air temperature forecast with 30 ensemble members.

out <- lapply(fc_date, function(dat) {
  
    idx <- which(fc_date == dat)
    
    fils <- list.files(fpath)
    fils <- fils[-c(grep("ens00", fils))]
    
    for( i in seq_len(length(fils))) {
      
      fid <- ncdf4::nc_open(file.path("./NOAA_FC_BARC", fils[i]))
      tim = ncvar_get(fid, "time")
      tunits = ncatt_get(fid, "time")
      lnam = tunits$long_name
      tustr <- strsplit(tunits$units, " ")
      step = tustr[[1]][1]
      tdstr <- strsplit(unlist(tustr)[3], "-")
      tmonth <- as.integer(unlist(tdstr)[2])
      tday <- as.integer(unlist(tdstr)[3])
      tyear <- as.integer(unlist(tdstr)[1])
      tdstr <- strsplit(unlist(tustr)[4], ":")
      thour <- as.integer(unlist(tdstr)[1])
      tmin <- as.integer(unlist(tdstr)[2])
      origin <- as.POSIXct(paste0(tyear, "-", tmonth,
                                  "-", tday, " ", thour, ":", tmin),
                           format = "%Y-%m-%d %H:%M", tz = "UTC")
      if (step == "hours") {
        tim <- tim * 60 * 60
      }
      if (step == "minutes") {
        tim <- tim * 60
      }
      time = as.POSIXct(tim, origin = origin, tz = "UTC")
      
      var_list <- lapply(fc_vars,function(x){
        data.frame(time = time, value = (ncdf4::ncvar_get(fid, x) -  273.15))
        })
      
      ncdf4::nc_close(fid)
      names(var_list) <- fc_vars
      
      mlt1 <- reshape::melt(var_list, id.vars = "time")
      mlt1 <- mlt1[, c("time", "L1", "value")]
      
      cnam <- paste0("mem", formatC(i, width = 2, format = "d", flag = "0"))
      if(i == 1) {
        df2 <- mlt1
        colnames(df2)[3] <- cnam
      } else {
        df2 <- merge(df2, mlt1, by = c(1,2))
        colnames(df2)[ncol(df2)] <- cnam
      }
      
    }
    return(df2)
  })
  
names(out) <- fc_date

noaa_fc <- reshape::melt(out[[1]][out[[1]]$L1 == "air_temperature", ], id.vars = c("time", "L1"))

noaa_fc$Date <- as.Date(noaa_fc$time)
noaa_fc <- plyr::ddply(noaa_fc, c("Date", "L1", "variable"), function(x) data.frame(value = mean(x$value, na.rm = TRUE)))
noaa_fc <- noaa_fc[noaa_fc$Date <= "2020-10-02", ]
colnames(noaa_fc) <- c("Forecast_date","Forecast_variable","Ensemble_member","value")

# Voila! We now have an object called "noaa_fc" which is a two-dimensional data frame containing a 
# 7-day-ahead NOAA air temperature forecast. Let's look at "noaa_fc".

head(noaa_fc)
# Forecast_date: this is the date for which temperature is forecasted
# Forecast_variable: this is the variable being forecasted
# Ensemble_member: this is an identifier for each member of the 30-member ensemble
# value: this is the value of the forecasted variable (in our case, degrees Celsius)

# Now we will plot the NOAA forecast.

# Data wrangling of observed air temperature data at Lake Barco so we can plot observed and 
# forecasted air temperature on one time series plot
lake_obs <- lake_df[lake_df$Date <= as.Date("2020-10-02") & lake_df$Date >= "2020-09-22", ]
lake_obs$wtemp[lake_obs$Date > as.Date("2020-09-25")] <- NA
lake_obs$airt[lake_obs$Date > as.Date("2020-09-25")] <- NA

# Defining another custom color palette :-)
l.cols <- RColorBrewer::brewer.pal(8, "Set2")[-c(1, 2)]

# Build plot
p2 <- ggplot() +
  geom_point(data = lake_obs, aes(Date, airt, color = "Observed air temp.")) +
  geom_line(data = noaa_fc, aes(Forecast_date, value, group = Ensemble_member, color = "Forecasted air temp."), alpha = 0.6)+
  geom_vline(xintercept = as.Date(fc_date), linetype = "dashed") +
  ylab("Temperature (\u00B0C)") +
  theme_bw(base_size = 12) +
  scale_color_manual(values = c("Observed air temp." = cols[1], "Forecasted air temp." = "gray"),
                     name = "",
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid","blank"),
                       shape = c(NA,16))))

# Render plot - this should match the time series plot in Activity B, Objective 9 - Driver Uncertainty
# IF you selected Lake Barco as your site
p2

# Data wrangling of NOAA air temperature ensemble forecast 
# We are re-formatting the NOAA air temperature ensemble forecast to make it easier
# to use in our forecasting exercises below
wid <- tidyr::pivot_wider(noaa_fc, c(Forecast_date, Forecast_variable), names_from = Ensemble_member, values_from = value)
wid <- as.data.frame(wid)
driv_mat <- as.matrix(wid[,3:32])

#4. Build multiple linear regression forecast model ----

# Build data frame to fit model
model_data <- data.frame(Date = lake_df$Date, 
                  wtemp = lake_df$wtemp,
                  airt = lake_df$airt,
                  wtemp_yday = NA)

# Populate column with water temperature from the previous day
model_data$wtemp_yday[-c(1:1)] <- model_data$wtemp[-c(nrow(model_data))]

# Fit multiple linear regression model using both water and air temperature
fit <- lm(model_data$wtemp ~ model_data$airt + model_data$wtemp_yday)
fit.summ <- summary(fit)

# View model coefficients and save them for our forecast later
coeffs <- round(fit$coefficients, 2)
coeffs

# View standard errors of estimated model coefficients and save them for
# our forecasts later
params.se <- fit.summ$coefficients[,2]
params.se

# Calculate model predictions
mod <- predict(fit, model_data)

# Assess model fit
r2 <- round(fit.summ$r.squared, 2) #R2
err <- mean(mod - model_data$wtemp, na.rm = TRUE) #mean bias
rmse <- round(sqrt(mean((mod - model_data$wtemp)^2, na.rm = TRUE)), 2) #RMSE

# Prepare data frames for plotting
lake_df2 <- lake_df[lake_df$Date > "2020-01-01", ]
  
pred <- data.frame(Date = model_data$Date,
                        Model = mod)
pred <- pred[pred$Date > "2020-01-01", ]
  
# Build plot of modeled and observed water temperature
p3 <- ggplot() +
    geom_point(data = lake_df2, aes(Date, wtemp, color = "Observed")) +
    geom_line(data = pred, aes(Date, Model, color = "Modeled")) +
    ylab("Temperature (\u00B0C)") +
    xlab("Time") +
    scale_color_manual(values = c( "Observed" = "black", "Modeled" = cols[6]),
                       name = "",
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid","blank"),
                         shape = c(NA,16)))) +
    theme_bw(base_size = 12) 

# Render plot - this should match the plot you see in the Shiny app Activity A, Objective 5 - 
# Improve Model for Forecasting IF you selected Lake Barco as your site
p3
  
#5. Run deterministic forecast ----

# Data wrangling to build forecast data frame
forecast_df <- model_data[model_data$Date <= as.Date("2020-10-02") & model_data$Date >= "2020-09-22", ] #subset dates
forecast_df$forecast <- NA #create forecast column
forecast_df$forecast[forecast_df$Date == fc_date] <- forecast_df$wtemp[forecast_df$Date == fc_date] #set initial conditions to current water temperature
  
# For a deterministic forecast, we will choose *one* ensemble member from the NOAA air temperature
# ensemble forecast and use that to drive our forecast model
mem01 <- noaa_fc %>%
  filter(Ensemble_member == "mem01" & Forecast_date >= "2020-09-26" & Forecast_date <= "2020-10-02")

# Insert forecasted air temperature values into forecast data frame air temperature column
forecast_df$airt[forecast_df$Date > fc_date] <- mem01$value #insert airtemp fc values here

# Run model. Each day, we forecast water temperature using forecasted air temperature and the 
# forecasted water temperature from the previous day. Notice that we are using the model coefficients
# from the model we fit in the previous section of the script.
fc_days <- which(forecast_df$Date >= fc_date)

for(i in fc_days[-1]) {
  forecast_df$forecast[i] <- forecast_df$airt[i] * coeffs[2] + forecast_df$forecast[i-1] * coeffs[3] + coeffs[1]
 } 

# Build plot
p4 <- ggplot() +
  geom_point(data = lake_obs, aes(Date, wtemp, color = "Observed water temp.")) +
  geom_line(data = forecast_df, aes(Date, forecast, color = "Forecasted water temp.")) +
  geom_vline(xintercept = as.Date(fc_date), linetype = "dashed") +
  ylab("Temperature (\u00B0C)") +
  theme_bw(base_size = 12) +
  scale_color_manual(values = c("Forecasted water temp." = cols[4],"Observed water temp." = cols[2]),
                     name = "",
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid","blank"),
                       shape = c(NA, 16))))

# Render plot - this should resemble the plot in the R Shiny app Activity B Overview, labeled
# "Water Temperature Forecast"; here, we are plotting the "Both" model, which uses both
# yesterday's water temperature and today's forecasted air temperature to forecast water
# temperature
p4

#6. Run forecast with process uncertainty ----

# Set value of noise that will be added to model
proc_unc <- 0.2 # Process Uncertainty Noise Std Dev.

# Setting up an empty matrix that we will fill with our water temperature predictions
mat <- matrix(NA, 8, 30) #8 rows for today + 7 forecast days, 30 columns for 30 NOAA ensemble members
mat[1, ] <- lake_df$wtemp[which(lake_df$Date == fc_date)]

# Run forecast. Here, instead of looping through days into the future, we are looping through
# ensemble members and indexing the previous row in the matrix when we need to grab the previous
# water temperature. We are also adding process uncertainty to each ensemble member prediction.
for(mem in 2:nrow(mat)) {
  mat[mem, ] <- mem01$value[mem-1] * coeffs[2] + mat[mem-1, ] * coeffs[3] + coeffs[1] + rnorm(30, 0, proc_unc)
}

# Data wrangling to get our ensemble forecast ready for plotting
fc_ens <- as.data.frame(mat)
colnames(fc_ens) <- colnames(wid[,3:32])
fc_ens$Forecast_date <- seq.Date(from = as.Date(fc_date), length.out = 8, by = 1)
fc_df <- reshape::melt(fc_ens, id.vars = "Forecast_date")
colnames(fc_df)[2] <- "Ensemble_member" 
fc_df$Forecast_variable <- "water temperature"

# Build plot
p5 <- ggplot() +
  geom_point(data = lake_obs, aes(Date, wtemp, color = "Observed water temp.")) +
  geom_line(data = fc_df, aes(Forecast_date, value, color = "Forecasted water temp.", group = Ensemble_member), alpha = 0.6) +
  geom_vline(xintercept = as.Date(fc_date), linetype = "dashed") +
  ylab("Temperature (\u00B0C)") +
  theme_bw(base_size = 12) +
  scale_color_manual(values = c("Forecasted water temp." = cols[6],"Observed water temp." = cols[2]),
                     name = "",
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid","blank"),
                       shape = c(NA, 16))))

# Render plot - this should resemble the water temperature forecast plot in the R Shiny app, 
# Activity B Objective 6 ("Both" model)
p5

# Calculate standard deviation of forecast to quantify uncertainty later
std.proc <- apply(mat, 1, sd)
df.proc <- data.frame(Date = seq.Date(from = as.Date(fc_date), length.out = 8, by = 1),
                  sd = std.proc, label = "Process")

#7. Run forecast with parameter uncertainty ----

# Generate parameter distributions based on parameter estimates for linear model
param.df <- data.frame(beta1 = rnorm(30, coeffs[1], params.se[1]),
                 beta2 = rnorm(30, coeffs[2], params.se[2]),
                 beta3 = rnorm(30, coeffs[3], params.se[3]))

# Plot parameter distributions
# Reshape data
plot.params <- reshape::melt(param.df)

# Build plot
p6 <- ggplot(plot.params) +
  geom_density(aes(value), fill = l.cols[4], alpha = 0.5) +
  facet_wrap(~variable, nrow = 1, scales = "free_x") +
  # scale_fill_manual(values = l.cols[idx]) +
  theme_bw(base_size = 16)

# Render plot - this should resemble the parameter distribution plot in the R Shiny app, 
# Activity B Objective 7 ("Both" model)
p6

# Setting up an empty matrix that we will fill with our water temperature predictions
mat <- matrix(NA, 8, 30) #8 rows for today + 7 forecast days, 30 columns for 30 NOAA ensemble members
mat[1, ] <- lake_df$wtemp[which(lake_df$Date == fc_date)]

# Run forecast. Here, instead of looping through days into the future, we are looping through
# ensemble members and indexing the previous row in the matrix when we need to grab the previous
# water temperature. We are also drawing from our parameter distributions so each ensemble member
# is using slightly different parameters.
for(mem in 2:nrow(mat)) {
  mat[mem, ] <- mem01$value[mem-1] * param.df$beta2 + mat[mem-1, ] * param.df$beta3 + param.df$beta1 
}

# Data wrangling to get our ensemble forecast ready for plotting
fc_ens <- as.data.frame(mat)
colnames(fc_ens) <- colnames(wid[,3:32])
fc_ens$Forecast_date <- seq.Date(from = as.Date(fc_date), length.out = 8, by = 1)
fc_df <- reshape::melt(fc_ens, id.vars = "Forecast_date")
colnames(fc_df)[2] <- "Ensemble_member" 
fc_df$Forecast_variable <- "water temperature"

# Build plot
p7 <- ggplot() +
  geom_point(data = lake_obs, aes(Date, wtemp, color = "Observed water temp.")) +
  geom_line(data = fc_df, aes(Forecast_date, value, color = "Forecasted water temp.", group = Ensemble_member), alpha = 0.6) +
  geom_vline(xintercept = as.Date(fc_date), linetype = "dashed") +
  ylab("Temperature (\u00B0C)") +
  theme_bw(base_size = 12) +
  scale_color_manual(values = c("Forecasted water temp." = cols[6],"Observed water temp." = cols[2]),
                     name = "",
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid","blank"),
                       shape = c(NA, 16))))

# Render plot - this should resemble the water temperature forecast plot in the R Shiny app, 
# Activity B Objective 7 ("Both" model)
p7

# Calculate standard deviation of forecast to quantify uncertainty later
std.param <- apply(mat, 1, sd)
df.param <- data.frame(Date = seq.Date(from = as.Date(fc_date), length.out = 8, by = 1),
                      sd = std.param, label = "Parameter")

#8. Run forecast with initial conditions uncertainty ----

# Generate initial conditions distribution 
curr_wt <- forecast_df[which(forecast_df$Date == fc_date),"wtemp"]
ic_uc <- 0.1 
ic_dist.df <- data.frame(value = rnorm(1000, curr_wt, ic_uc))

# Plot initial conditions distribution
# Set plot window limits
xlims <- c(curr_wt -1.5, curr_wt + 1.5)
ylims <- c(0,7)

#Build plot
p8 <- ggplot() +
  # geom_vline(data = df, aes(xintercept = x, color = label)) +
  geom_vline(xintercept = curr_wt) +
  geom_density(data = ic_dist.df, aes(value), fill = l.cols[2], alpha = 0.3) +
  xlab("Temperature (\u00B0C)") +
  ylab("Density") +
  coord_cartesian(xlim = xlims, ylim = ylims) +
  theme_bw(base_size = 18)

# Render plot - this should resemble the initial condition distribution plot in the R Shiny app, 
# Activity B Objective 8 ("Both" model)
p8

# Randomly sample 30 values to use for our forecast
ic_samp <- sample(ic_dist.df$value, 30, replace = TRUE)

# Setting up an empty matrix that we will fill with our water temperature predictions
mat <- matrix(NA, 8, 30) #8 rows for today + 7 forecast days, 30 columns for 30 NOAA ensemble members
mat[1, ] <- ic_samp

# Run forecast. Here, instead of looping through days into the future, we are looping through
# ensemble members and indexing the previous row in the matrix when we need to grab the previous
# water temperature. We are also drawing from our initial condition distribution so each ensemble member
# is using a slightly different initial condition.
for(mem in 2:nrow(mat)) {
  mat[mem, ] <- mem01$value[mem-1] * coeffs[2] + mat[mem-1, ] * coeffs[3] + coeffs[1] 
}

# Data wrangling to get our ensemble forecast ready for plotting
fc_ens <- as.data.frame(mat)
colnames(fc_ens) <- colnames(wid[,3:32])
fc_ens$Forecast_date <- seq.Date(from = as.Date(fc_date), length.out = 8, by = 1)
fc_df <- reshape::melt(fc_ens, id.vars = "Forecast_date")
colnames(fc_df)[2] <- "Ensemble_member" 
fc_df$Forecast_variable <- "water temperature"

# Build plot
p9 <- ggplot() +
  geom_point(data = lake_obs, aes(Date, wtemp, color = "Observed water temp.")) +
  geom_line(data = fc_df, aes(Forecast_date, value, color = "Forecasted water temp.", group = Ensemble_member), alpha = 0.6) +
  geom_vline(xintercept = as.Date(fc_date), linetype = "dashed") +
  ylab("Temperature (\u00B0C)") +
  theme_bw(base_size = 12) +
  scale_color_manual(values = c("Forecasted water temp." = cols[6],"Observed water temp." = cols[2]),
                     name = "",
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid","blank"),
                       shape = c(NA, 16))))

# Render plot - this should resemble the water temperature forecast plot in the R Shiny app, 
# Activity B Objective 8 ("Both" model)
p9

# Calculate standard deviation of forecast to quantify uncertainty later
std.ic <- apply(mat, 1, sd)
df.ic <- data.frame(Date = seq.Date(from = as.Date(fc_date), length.out = 8, by = 1),
                       sd = std.ic, label = "Initial Condition")

#9. Run forecast with driver uncertainty ----

# Setting up an empty matrix that we will fill with our water temperature predictions
mat <- matrix(NA, 8, 30) #8 rows for today + 7 forecast days, 30 columns for 30 NOAA ensemble members
mat[1, ] <- lake_df$wtemp[which(lake_df$Date == fc_date)]

# Run forecast. Here, instead of looping through days into the future, we are looping through
# ensemble members and indexing the previous row in the matrix when we need to grab the previous
# water temperature. So we end up with a forecast corresponding to every member of the 30-member
# NOAA ensemble forecast.
for(mem in 2:nrow(mat)) {
  mat[mem, ] <- driv_mat[mem, ] * coeffs[2] + mat[mem-1, ] * coeffs[3] + coeffs[1]
}

# Data wrangling to get our ensemble forecast ready for plotting
fc_ens <- as.data.frame(mat)
colnames(fc_ens) <- colnames(wid[,3:32])
fc_ens$Forecast_date <- seq.Date(from = as.Date(fc_date), length.out = 8, by = 1)
fc_df <- reshape::melt(fc_ens, id.vars = "Forecast_date")
colnames(fc_df)[2] <- "Ensemble_member" 
fc_df$Forecast_variable <- "water temperature"

# Build plot
p10 <- ggplot() +
  geom_point(data = lake_obs, aes(Date, wtemp, color = "Observed water temp.")) +
  geom_line(data = fc_df, aes(Forecast_date, value, color = "Forecasted water temp.", group = Ensemble_member), alpha = 0.6) +
  geom_vline(xintercept = as.Date(fc_date), linetype = "dashed") +
  ylab("Temperature (\u00B0C)") +
  theme_bw(base_size = 12) +
  scale_color_manual(values = c("Forecasted water temp." = cols[6],"Observed water temp." = cols[2]),
                     name = "",
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid","blank"),
                       shape = c(NA, 16))))

# Render plot - this should resemble the water temperature forecast plot in the R Shiny app, 
# Activity B Objective 9 ("Both" model)
p10

# Calculate standard deviation of forecast to quantify uncertainty later
std.driv <- apply(mat, 1, sd)
df.driv <- data.frame(Date = seq.Date(from = as.Date(fc_date), length.out = 8, by = 1),
                       sd = std.driv, label = "Driver")

#10. Run forecast with all uncertainties -----------

# Setting up an empty matrix that we will fill with our water temperature predictions
mat <- matrix(NA, 8, 30) #8 rows for today + 7 forecast days, 30 columns for 30 NOAA ensemble members
mat[1, ] <- ic_samp #Notice that we are drawing from our initial condition distribution

# Run forecast. Here, we add process uncertainty, use different values of parameters and
# initial conditions for each ensemble member, and use a different NOAA ensemble member to
# drive each of our water temperature forecast ensemble members. So we are incorporating
# four different possible sources of uncertainty.
for(mem in 2:nrow(mat)) {
  mat[mem, ] <- driv_mat[mem, ] * param.df$beta2 + mat[mem-1, ] * param.df$beta3 + param.df$beta1 + rnorm(30, 0, proc_unc)
}

# Data wrangling to get our ensemble forecast ready for plotting
fc_ens <- as.data.frame(mat)
colnames(fc_ens) <- colnames(wid[,3:32])
fc_ens$Forecast_date <- seq.Date(from = as.Date(fc_date), length.out = 8, by = 1)
fc_df <- reshape::melt(fc_ens, id.vars = "Forecast_date")
colnames(fc_df)[2] <- "Ensemble_member" 
fc_df$Forecast_variable <- "water temperature"

# Build plot
p11 <- ggplot() +
  geom_point(data = lake_obs, aes(Date, wtemp, color = "Observed water temp.")) +
  geom_line(data = fc_df, aes(Forecast_date, value, color = "Forecasted water temp.", group = Ensemble_member), alpha = 0.6) +
  geom_vline(xintercept = as.Date(fc_date), linetype = "dashed") +
  ylab("Temperature (\u00B0C)") +
  theme_bw(base_size = 12) +
  scale_color_manual(values = c("Forecasted water temp." = cols[4],"Observed water temp." = cols[2]),
                     name = "",
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid","blank"),
                       shape = c(NA, 16))))

# Render plot - this should resemble the water temperature forecast plot in the R Shiny app, 
# Activity C Objective 10 ("Both" model)
p11

# Quantify uncertainty

# Create a data frame that combines all our calculations of the contributions of
# different sources of uncertainty (process, parameter, initial condition, driver)
quantfc.df <- rbind(df.proc,df.param,df.ic,df.driv)

# Set another custom plot color palette
cols2 <- ggthemes::ggthemes_data$colorblind$value

# Plot the contribution of each source of uncertainty to total forecast uncertainty
# Build plot
p12 <- ggplot() +
  geom_bar(data = quantfc.df, aes(Date, sd, fill = label), stat = "identity", position = "stack") +
  ylab("Standard Deviation (\u00B0C)") +
  scale_fill_manual(values = c("Process" = cols2[1], "Parameter" = cols2[2], "Initial Condition" = cols2[3],
                               "Driver" = cols2[4], "Total" = cols2[5])) +
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
  labs(fill = "Uncertainty") +
  theme_bw(base_size = 12)

# Render plot - this should resemble the uncertainty quantification plot in the R Shiny
# app, Activity C, Objective 10 ("Both" model)
p12

# Congratulations! You have quantified all the uncertainty. Now, have a nap :-)
