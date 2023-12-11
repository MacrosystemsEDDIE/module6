# Load required libraries
suppressPackageStartupMessages(library(shiny, quietly = TRUE))
suppressPackageStartupMessages(library(shinyjs, quietly = TRUE))
suppressPackageStartupMessages(library(DT, quietly = TRUE))
suppressPackageStartupMessages(library(sf, quietly = TRUE))
suppressPackageStartupMessages(library(leaflet, quietly = TRUE))
suppressPackageStartupMessages(library(plotly, quietly = TRUE))
suppressPackageStartupMessages(library(ggpubr, quietly = TRUE))
suppressPackageStartupMessages(library(kableExtra, quietly = TRUE))
suppressPackageStartupMessages(library(shinyalert, quietly = TRUE))
suppressPackageStartupMessages(library(shinyBS, quietly = TRUE))
suppressPackageStartupMessages(library(shinydashboard, quietly = TRUE))
suppressPackageStartupMessages(library(rintrojs, quietly = TRUE))
suppressPackageStartupMessages(library(slickR, quietly = TRUE))
suppressPackageStartupMessages(library(sortable, quietly = TRUE))
suppressPackageStartupMessages(library(ncdf4, quietly = TRUE))
suppressPackageStartupMessages(library(ggplot2, quietly = TRUE))
suppressPackageStartupMessages(library(stringr, quietly = TRUE))
suppressPackageStartupMessages(library(hover, quietly = TRUE))
suppressPackageStartupMessages(library(lubridate, quietly = TRUE))
suppressPackageStartupMessages(library(tidyverse, quietly = TRUE))
suppressPackageStartupMessages(library(glue, quietly = TRUE))

# Enable bookmarking
enableBookmarking(store = "url")

# Help documentation
help_text <- read.csv("data/help_text.csv", row.names = 1)

# Load text input
module_text <- read.csv("data/module_text.csv", row.names = 1, header = FALSE)

# colors for theme
obj_bg <- "#D4ECE1"
ques_bg <- "#B8E0CD"
nav_bg <- "#DDE4E1"
nav_butt <- "#31ED92"
nav_txt <- "#000000" # white = #fff; black = #000000
slider_col <- "#2CB572"

# Change maximum upload file size
options(shiny.maxRequestSize=30*1024^2)

# Colors for plots
scale_colour_discrete <- ggthemes::scale_colour_colorblind
scale_fill_discrete <- ggthemes::scale_fill_colorblind
cols <- c("#1B9E77", "#D95F02"  ,"#2580F3" ,"#E7298A", "#66A61E", "#E6AB02","#A6761D", "#666666")
cols2 <- ggthemes::ggthemes_data$colorblind$value
l.cols <- RColorBrewer::brewer.pal(8, "Set2")[-c(1, 2)]
p.cols <- RColorBrewer::brewer.pal(12, "Paired")

# Plot saving
png_dpi <- 120
p_wid <- 213
p_hei <- 120
p_units <- "mm"
l_siz <- 0.6
p_siz <- 1.6

# Functions required
source("R/download_phenocam.R")
source("R/get_html.R")
source("R/textAreaInput2.R")

# Help documentation
help_text <- read.csv("data/help_text.csv", row.names = 1)

# Load and format questions
quest <- read.csv("data/student_questions.csv", row.names = 1)
qid <- row.names(quest)
idx <- which(grepl("Name of selected ", quest$Question))
idx2 <- which(grepl("Elevation", quest$Question))

# Read in assessment questions
quest <- read.csv("data/student_questions.csv", row.names = 1)

# Slides
recap_slides <- list.files("www/shiny_slides", full.names = TRUE)
model_slides <- list.files("www/model_slides", full.names = TRUE)
proc_uc_slides <- list.files("www/proc_uc_slides", full.names = TRUE)
param_uc_slides <- list.files("www/param_uc_slides", full.names = TRUE)
ic_uc_slides <- list.files("www/ic_uc_slides", full.names = TRUE)
driver_uc_slides <- list.files("www/driver_uc_slides", full.names = TRUE)

# Load in sp format with coordinates
neon_sites <- readRDS("data/neon_sites.rds")
neon_sites <- neon_sites[which(neon_sites$siteID %in% c("BARC", "PRPO", "LIRO")), ]
neon_sites$uid <- paste0("M", seq_len(nrow(neon_sites)))

#Load in the dataframe
neon_sites_df <- read.csv("data/neon_sites.csv")
neon_sites_df$long <- round(neon_sites_df$long, 3)
neon_sites_df$lat <- round(neon_sites_df$lat, 3)
neon_sites_df <- neon_sites_df[which(neon_sites_df$siteID %in% c("BARC", "PRPO", "LIRO")), ]
neon_sites_df$uid <- paste0("M", seq_len(nrow(neon_sites_df))) # For leaflet map

# Add type labels
neon_sites$type[which(neon_sites$siteID %in% (neon_sites_df$siteID[neon_sites_df$type == "Aquatic"]))] <- "Aquatic"
neon_sites$type[which(neon_sites$siteID %in% (neon_sites_df$siteID[neon_sites_df$type == "Terrestrial"]))] <- "Terrestrial"

# Subset to aquatic
neon_sites <- neon_sites[neon_sites$type == "Aquatic", ]
neon_sites_df <- neon_sites_df[neon_sites_df$type == "Aquatic", ]

# Reference for downloading variables
neon_vars <- read.csv("data/neon_variables.csv")

# Statistics
stats0 <- list("Minimum" = "Min.", "Maximum" = "Max.", "Mean" = "Mean")
q6_table <- data.frame(
  Mean = rep(NA, 1),
  Min = rep(NA, 1),
  Max = rep(NA, 1), row.names = c("Water temperature")
)
q8_table <- data.frame(
  Mean = rep(NA, 1),
  Min = rep(NA, 1),
  Max = rep(NA, 1), row.names = c("Air temperature")
)

stats <- list("Minimum" = "Min.", "1st Quartile" = "1st Qu.", "Median" = "Median", "Mean" = "Mean", "3rd Quartile" = "3rd Qu.", "Maximum" = "Max.", "Standard Deviation" = "sd")

mytheme <- theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
                 axis.text.x=element_text(size=18, colour='black'), axis.text.y=element_text(size=18, colour='black'),
                 axis.title.x=element_text(size=18), axis.title.y=element_text(size=18),
                 strip.text.x = element_text(size=14), strip.text.y = element_text(size=14),
                 panel.background = element_rect(fill = NA, color = "black"),
                 panel.grid.major = element_line(colour = "gray"),
                 legend.text=element_text(size=16),
                 legend.title = element_text(size = 20))


png_theme <- theme(legend.position = "bottom",
                   legend.text = element_text(size = 14),
                   legend.title = element_text(size = 14))

# Linear regression variables ----
lin_reg_vars <- read.csv("data/multiple_linear_regression_variables.csv",
                         fileEncoding = "UTF-8-BOM")

# Forecast Date
fc_date <- "2020-09-25"

# Uncertainty sources to include
uc_sources <- c("Process", "Parameter", "Initial Conditions", "Driver", "Total")

# Add last update time
app_time <- format(file.info("ui.R")$mtime, "%Y-%m-%d")
app_update_txt <- paste0("This app was last updated on: ", app_time)

# Tab names for updating buttons
tab_names <- read.csv("data/tab_names.csv", fileEncoding = "UTF-8-BOM")

# Model names
mod_names <- c("Pers", "Wtemp", "Atemp", "Both")

# Dam levels
dam_lev <- c("Surface", "Bottom")

# Scenario Forecast #1
set.seed(123)
scen_fc1 <- data.frame(Date = seq.Date(as.Date("2021-08-16"), as.Date("2021-08-22"), by = 1),
                       surftemp = c(9.6, 9.8, 10.1, 11.1, 11.5, 11.4, 11.8),
                       bottemp = c(7.3, 7.6, 7.9, 8.2, 8.6, 9.2, 9.4))
scen_fc1$bottemp <- scen_fc1$bottemp + 1.9

bsd <- rnorm(nrow(scen_fc1), 1.2, 0.1) - 0.9
ssd <- rnorm(nrow(scen_fc1), 1.25, 0.18) - 0.9

scen_fc1$surf_uci <- scen_fc1$surftemp + ssd[order(ssd)]
scen_fc1$surf_lci <- scen_fc1$surftemp - ssd[order(ssd)]

scen_fc1$bot_uci <- scen_fc1$bottemp + bsd[order(bsd)]
scen_fc1$bot_lci <- scen_fc1$bottemp - bsd[order(bsd)]

# Scenario Forecast #2
scen_fc2 <- data.frame(Date = seq.Date(as.Date("2021-08-16"), as.Date("2021-08-22"), by = 1),
                       surftemp = c(9.6, 9.8, 10.1, 11.1, 11.5, 11.4, 11.8),
                       bottemp = c(7.3, 7.6, 7.9, 8.2, 8.6, 9.2, 9.4))
scen_fc2$bottemp <- scen_fc2$bottemp + 1.9
set.seed(123)
bsd <- rnorm(nrow(scen_fc2), 1.75, 0.6) - 0.9
ssd <- rnorm(nrow(scen_fc2), 1.2, 0.25) - 0.9

scen_fc2$surf_uci <- scen_fc2$surftemp + ssd[order(ssd)]
scen_fc2$surf_uci[5] <- scen_fc2$surftemp[5] + max(ssd)
scen_fc2$surf_lci <- scen_fc2$surftemp - ssd[order(ssd)]
scen_fc2$surf_lci[5] <- scen_fc2$surftemp[5] - max(ssd)

scen_fc2$bot_uci <- scen_fc2$bottemp + bsd[order(bsd)]
scen_fc2$bot_lci <- scen_fc2$bottemp - bsd[order(bsd)]

# Icons
neonIcons <- iconList(
  Aquatic = makeIcon("icons/water-icon.png", iconWidth = 28, iconHeight = 28),
  Terrestrial = makeIcon("icons/mountain-icon.png", iconWidth =  28, iconHeight = 28)
)

# Reference for downloading variables
neon_vars <- read.csv("data/neon_variables.csv")
met_pars <- read.csv("data/met_params.csv", fileEncoding = "UTF-8-BOM")

# a table container with complex header
sketch1 = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(colspan = 2, 'Slope (m)'),
      th(colspan = 2, 'Intercept (b)')
    ),
    tr(
      lapply(rep(c('Mean', 'Std. Dev.'), 2), th)
    )
  )
))

# function to run deterministic forecasts
run_deterministic_forecast <- function(model, data, airtemp_forecast, 
                                       lr_pars3, lr_pars2, mlr_pars,
                                       model_table){
  
  idx <- model
  
  dat <- data.frame(Date = data$Date, 
                    wtemp = data$wtemp,
                    airt = data$airt,
                    wtemp_yday = NA,
                    airt_yday = NA)
  
  dat$wtemp_yday[-c(1:model_table$lag[idx])] <- dat$wtemp[-c((nrow(dat)+1-model_table$lag[idx]):nrow(dat))]
  dat$airt_yday[-c(1:model_table$lag[idx])] <- dat$airt[-c((nrow(dat)+1-model_table$lag[idx]):nrow(dat))]
  
  lag_date <- (as.Date(fc_date) + model_table$lag[idx])
  mn_date <- (as.Date(fc_date) + 1)
  
  dat <- dat[dat$Date <= as.Date("2020-10-02") & dat$Date >= "2020-09-22", ]
  dat$wtemp[dat$Date > fc_date] <- NA
  dat$forecast <- NA
  dat$forecast[dat$Date == fc_date] <- dat$wtemp[dat$Date == fc_date]
  dat$airt[dat$Date > fc_date] <- airtemp_forecast$value[2:8]
  dat$wtemp_yday[dat$Date > lag_date] <- NA
  dat$airt_yday[dat$Date > mn_date] <- NA
  
  df <- data.frame(Date = seq.Date(as.Date("2020-09-22"), as.Date("2020-10-02"), by = 1))
  df <- merge(dat, df, by = "Date", all.y = TRUE)
  
  fc_days <- which(df$Date >= fc_date)
  if(model == 3) {
    for(i in fc_days[-1]) {
      df$forecast[i] <- df$airt[i] * lr_pars3$m[1] + lr_pars3$b[1]
    }
  } else if(model == 1) {
    for(i in fc_days[-1]) {
      df$forecast[i] <- df$forecast[i-1]
    }
  } else if(model == 2) {
    for(i in fc_days[-1]) {
      df$forecast[i] <- df$forecast[i-1] * lr_pars2$m[1] + lr_pars2$b[1]
    }
  } else if(model == 4) {
    for(i in fc_days[-1]) {
      df$forecast[i] <- df$forecast[i-1] * mlr_pars$b1_est[1] + df$airt[i] * mlr_pars$b2_est[1] + mlr_pars$b0_est[1]
    }
  }
  return(df)
}

# function to run process forecasts
run_process_forecast <- function(model, data, airtemp_forecast, 
                                       lr_pars3, lr_pars2, mlr_pars,
                                       model_table, sigmas){
  
  idx <- model
  
  dat <- data.frame(Date = data$Date, wtemp = data$wtemp,
                    airt = data$airt,
                    wtemp_yday = NA,
                    airt_yday = NA)
  
  dat$wtemp_yday[-c(1:model_table$lag[idx])] <- dat$wtemp[-c((nrow(dat)+1-model_table$lag[idx]):nrow(dat))]
  dat$airt_yday[-c(1:model_table$lag[idx])] <- dat$airt[-c((nrow(dat)+1-model_table$lag[idx]):nrow(dat))]
  
  lag_date <- (as.Date(fc_date) + model_table$lag[idx])
  mn_date <- (as.Date(fc_date) + 1)
  
  dat <- dat[dat$Date <= as.Date("2020-10-02") & dat$Date >= "2020-09-22", ]
  dat$wtemp[dat$Date > fc_date] <- NA
  dat$forecast <- NA
  dat$forecast[dat$Date == fc_date] <- dat$wtemp[dat$Date == fc_date]
  dat$airt[dat$Date > fc_date] <- airtemp_forecast$value[2:8]
  dat$wtemp_yday[dat$Date > lag_date] <- NA
  dat$airt_yday[dat$Date > mn_date] <- NA
  
  df <- data.frame(Date = seq.Date(as.Date("2020-09-22"), as.Date("2020-10-02"), by = 1))
  df <- merge(dat, df, by = "Date", all.y = TRUE)
  
  mat <- matrix(NA, 8, 100)
  mat[1, ] <- df$wtemp[which(df$Date == fc_date)]
  df <- df[(df$Date >= fc_date), ]

  for(mem in 2:nrow(mat)) {
    if(idx == 3) {
      Wt <- sigmas[3,2]
      mat[mem, ] <- df$airt[mem] * lr_pars3$m[1] + lr_pars3$b[1] + rnorm(100, 0, Wt)
    } else if(idx == 1) {
      Wt <- sigmas[1,2]
      mat[mem, ] <- mat[mem-1, ] + rnorm(100, 0, Wt)
    } else if(idx == 2) {
      Wt <- sigmas[2,2]
      mat[mem, ] <- mat[mem-1, ] * lr_pars2$m[1] + lr_pars2$b[1] + rnorm(100, 0, Wt)
    } else if(idx == 4) {
      Wt <- sigmas[4,2]
      mat[mem, ] <- mat[mem-1, ] * mlr_pars$b1_est[1] + df$airt[mem] * mlr_pars$b2_est[1] + mlr_pars$b0_est[1] + rnorm(100, 0, Wt)
    }
  }
  
  # Calculate distributions
  dat <- apply(mat, 1, function(x){
    quantile(x, c(0.05, 0.5, 0.95))
  })
  dat <- as.data.frame(t(dat))
  colnames(dat) <- paste0("p", gsub("%", "", colnames(dat)))
  dat$Date <- seq.Date(from = as.Date(fc_date), length.out = 8, by = 1)
  dat$Level <- as.character(idx)

  df2 <- as.data.frame(mat)
  df2$Date <- seq.Date(from = as.Date(fc_date), length.out = 8, by = 1)
  mlt <- reshape::melt(df2, id.vars = "Date")
  mlt$Level <- as.character(idx)
  
  return(list(df = df, dat = dat, mlt = mlt))
  
}

# create action buttons for table function - delete this eventually
create_btns <- function(x, label) {
  x %>%
    purrr::map_chr(~
                     paste0(
                       '<button id=',.x,' type="button" class="btn btn-default action-button">',label,'</button>'
                     ))
}

# function to run parameter forecasts
run_param_forecast <- function(model, data, airtemp_forecast, 
                                 param_dist,
                                 model_table){
  
idx <- model

pars <- param_dist[[idx]]

if(idx != 1) {
  pars <- pars[sample(1:nrow(pars), size = 100), ]
}

dat <- data.frame(Date = data$Date, wtemp = data$wtemp,
                  airt = data$airt,
                  wtemp_yday = NA,
                  airt_yday = NA)

dat$wtemp_yday[-c(1:model_table$lag[idx])] <- dat$wtemp[-c((nrow(dat)+1-model_table$lag[idx]):nrow(dat))]
dat$airt_yday[-c(1:model_table$lag[idx])] <- dat$airt[-c((nrow(dat)+1-model_table$lag[idx]):nrow(dat))]

lag_date <- (as.Date(fc_date) + model_table$lag[idx])
mn_date <- (as.Date(fc_date) + 1)

dat <- dat[dat$Date <= as.Date("2020-10-02") & dat$Date >= "2020-09-22", ]
dat$wtemp[dat$Date > fc_date] <- NA
dat$forecast <- NA
dat$forecast[dat$Date == fc_date] <- dat$wtemp[dat$Date == fc_date]
dat$airt[dat$Date > fc_date] <- airtemp_forecast$value[2:8]
dat$wtemp_yday[dat$Date > lag_date] <- NA
dat$airt_yday[dat$Date > mn_date] <- NA

df <- data.frame(Date = seq.Date(as.Date("2020-09-22"), as.Date("2020-10-02"), by = 1))
df <- merge(dat, df, by = "Date", all.y = TRUE)

mat <- matrix(NA, 8, 100)
mat[1, ] <- df$wtemp[which(df$Date == fc_date)]
df <- df[(df$Date >= fc_date), ]

for(mem in 2:nrow(mat)) {
  
  if(idx == 3) {
    mat[mem, ] <- df$airt[mem] * pars$m + pars$b
  } else if(idx == 1) {
    mat[mem, ] <- mat[mem-1, ]
  } else if(idx == 2) {
    mat[mem, ] <- mat[mem-1, ] * pars$m + pars$b
  } else if(idx == 4) {
    mat[mem, ] <- mat[mem-1, ] * pars$beta1 + df$airt[mem] * pars$beta2 + pars$beta0
    
  }
}

# Calculate distributions
dat <- apply(mat, 1, function(x){
  quantile(x, c(0.05, 0.5, 0.95))
})
dat <- as.data.frame(t(dat))
colnames(dat) <- paste0("p", gsub("%", "", colnames(dat)))
dat$Date <- seq.Date(from = as.Date(fc_date), length.out = 8, by = 1)
dat$Level <- as.character(idx)

df2 <- as.data.frame(mat)
df2$Date <- seq.Date(from = as.Date(fc_date), length.out = 8, by = 1)
mlt <- reshape::melt(df2, id.vars = "Date")
mlt$Level <- as.character(idx)

return(list(mlt = mlt, dat = dat))

}

# function to run ic forecasts
run_ic_forecast <- function(model, data, airtemp_forecast, 
                                 lr_pars3, lr_pars2, mlr_pars,
                                 model_table){
  
  idx <- model
  
  dat <- data.frame(Date = data$Date, wtemp = data$wtemp,
                    airt = data$airt,
                    wtemp_yday = NA,
                    airt_yday = NA)
  
  dat$wtemp_yday[-c(1:model_table$lag[idx])] <- dat$wtemp[-c((nrow(dat)+1-model_table$lag[idx]):nrow(dat))]
  dat$airt_yday[-c(1:model_table$lag[idx])] <- dat$airt[-c((nrow(dat)+1-model_table$lag[idx]):nrow(dat))]
  
  lag_date <- (as.Date(fc_date) + model_table$lag[idx])
  mn_date <- (as.Date(fc_date) + 1)
  
  
  dat <- dat[dat$Date <= as.Date("2020-10-02") & dat$Date >= "2020-09-22", ]
  dat$wtemp[dat$Date > fc_date] <- NA
  dat$forecast <- NA
  dat$forecast[dat$Date == fc_date] <- dat$wtemp[dat$Date == fc_date]
  dat$airt[dat$Date > fc_date] <- airtemp_forecast$value[2:8]
  dat$wtemp_yday[dat$Date > lag_date] <- NA
  dat$airt_yday[dat$Date > mn_date] <- NA
  
  df <- data.frame(Date = seq.Date(as.Date("2020-09-22"), as.Date("2020-10-02"), by = 1))
  df <- merge(dat, df, by = "Date", all.y = TRUE)
  
  mat <- matrix(NA, 8, 100)
  mat[1, ] <- rnorm(100, df$wtemp[which(df$Date == fc_date)], sd = 0.1) #0.1 is sensor error value
  df <- df[(df$Date >= fc_date), ]

  for(mem in 2:nrow(mat)) {
    if(idx == 3) {
      mat[1, ] <- df$wtemp[which(df$Date == fc_date)] #no IC uc here!
      mat[mem, ] <- df$airt[mem] * lr_pars3$m[1] + lr_pars3$b[1]
    } else if(idx == 1) {
      mat[mem, ] <- mat[mem-1, ]
    } else if(idx == 2) {
      mat[mem, ] <- mat[mem-1, ] * lr_pars2$m[1] + lr_pars2$b[1]
    } else if(idx == 4) {
      mat[mem, ] <- mat[mem-1, ] * mlr_pars$b1_est[1] + df$airt[mem] * mlr_pars$b2_est[1] + mlr_pars$b0_est[1]
    }
  }
  
  # Calculate distributions
  dat <- apply(mat, 1, function(x){
    quantile(x, c(0.05, 0.5, 0.95))
  })
  dat <- as.data.frame(t(dat))
  colnames(dat) <- paste0("p", gsub("%", "", colnames(dat)))
  dat$Date <- seq.Date(from = as.Date(fc_date), length.out = 8, by = 1)
  dat$Level <- as.character(idx)

  df2 <- as.data.frame(mat)
  df2$Date <- seq.Date(from = as.Date(fc_date), length.out = 8, by = 1)
  mlt <- reshape::melt(df2, id.vars = "Date")
  mlt$Level <- as.character(idx)
  
  return(list(df = df, mlt = mlt, dat = dat))
  
}

# function to run driver forecasts
run_driver_forecast <- function(model, data, airtemp_forecast, 
                            lr_pars3, lr_pars2, mlr_pars,
                            model_table, airtemp_forecast_data){

  mlt <- airtemp_forecast
  mlt$Date <- as.Date(mlt$time)
  mlt <- plyr::ddply(mlt, c("Date", "L1", "variable"), function(x) data.frame(value = mean(x$value, na.rm = TRUE)))
  mlt <- mlt[mlt$Date <= "2020-10-02", ]
  
  wid <- tidyr::pivot_wider(mlt, c(Date, L1), names_from = variable, values_from = value)
  wid <- as.data.frame(wid)
  
  idx <- model
  
  dat <- data.frame(Date = data$Date, wtemp = data$wtemp,
                    airt = data$airt,
                    wtemp_yday = NA,
                    airt_yday = NA)
  
  dat$wtemp_yday[-c(1:model_table$lag[idx])] <- dat$wtemp[-c((nrow(dat)+1-model_table$lag[idx]):nrow(dat))]
  dat$airt_yday[-c(1:model_table$lag[idx])] <- dat$airt[-c((nrow(dat)+1-model_table$lag[idx]):nrow(dat))]
  
  lag_date <- (as.Date(fc_date) + model_table$lag[idx])
  mn_date <- (as.Date(fc_date) + 1)
  
  df <- airtemp_forecast_data[[1]]
  
  mat <- matrix(NA, 8, 30)
  mat[1, ] <- df$wtemp[which(df$Date == fc_date)]
  df <- df[(df$Date >= fc_date), ]
  idx <- model
  driv_mat <- sapply(1:30, function(x) airtemp_forecast_data[[x]]$airt[airtemp_forecast_data[[x]]$Date >= fc_date] )
  
  for(mem in 2:nrow(mat)) {
    if(idx == 1) {
      mat[mem, ] <- mat[mem-1, ]
    } else if(idx == 2) {
      mat[mem, ] <- mat[mem-1, ] * lr_pars2$m[1] + lr_pars2$b[1]
    } else if(idx == 3) {
      mat[mem, ] <- driv_mat[mem, ] * lr_pars3$m[1] + lr_pars3$b[1]
    } else if(idx == 4) {
      mat[mem, ] <- mat[mem-1, ] * mlr_pars$b1_est[1] + driv_mat[mem, ] * mlr_pars$b2_est[1] + mlr_pars$b0_est[1]
    }
  }
  
  # Calculate distributions
  dat <- apply(mat, 1, function(x){
    quantile(x, c(0.05, 0.5, 0.95))
  })
  dat <- as.data.frame(t(dat))
  colnames(dat) <- paste0("p", gsub("%", "", colnames(dat)))
  dat$Date <- seq.Date(from = as.Date(fc_date), length.out = 8, by = 1)
  dat$Level <- as.character(idx)
  
  df2 <- as.data.frame(mat)
  df2$Date <- seq.Date(from = as.Date(fc_date), length.out = 8, by = 1)
  mlt <- reshape::melt(df2, id.vars = "Date")
  mlt$Level <- as.character(idx)

  return(list(dat = dat, mlt = mlt, df = df))
  
}


# end
