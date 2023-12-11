# # Title: Example plot for perfect linear relationship
# # Author: Mary Lofton
# # Date: 03AUG23
# 
# library(tidyverse)
# library(lubridate)
# 
# # Read in air temperature data for Lake Barco
# # We are calling the air temperature data "xvar" because we are using it as a predictor of water temperature
# xvar <- read.csv("./app/data/neon/BARC_airt_celsius.csv")
# 
# # Data wrangling to reformat dates
# xvar$Date <- as.Date(xvar[, 1])
# 
# # Data wrangling to calculate daily average air temperature
# xvar <- plyr::ddply(xvar, c("Date"), function(x) mean(x[, 2], na.rm = TRUE))
# 
# # Read in surface water temperature data
# # We are calling this "yvar" because it is the variable we are trying to predict
# yvar <- read.csv("./app/data/neon/BARC_wtemp_celsius.csv")
# 
# # Data wrangling to reformat dates
# yvar$Date <- as.Date(yvar[, 1])
# 
# # Data wrangling to subset water temperature to only surface temperature and calculate daily average
# yvar <- yvar[yvar[, 2] == min(yvar[, 2], na.rm = TRUE), c(1, 3)] # subset to Surface water temperature
# yvar <- plyr::ddply(yvar, c("Date"), function(y) mean(y[, 2], na.rm = TRUE)) # Daily average
# 
# # Combine air temperature and surface water temperature into one dataframe
# lake_df <- merge(xvar, yvar, by = "Date")
# 
# # Rename columnns to sensible names after joining
# colnames(lake_df) <- c("date", "airt", "wtemp")
# 
# # Limit data to complete cases (rows with both air and water temperature available)
# lake_df$airt[is.na(lake_df$wtemp)] <- NA
# lake_df$wtemp[is.na(lake_df$airt)] <- NA
# lake_df <- lake_df[which(complete.cases(lake_df)),]
# 
# model_data <- lake_df
# 
# mx <- max(model_data$wtemp, na.rm = TRUE) + 2
# mn <- min(model_data$wtemp, na.rm = TRUE) - 2
# 
# sgmnt <- data.frame(x = mn, xend = mx, y = mn, yend = mx)
# 
# 
# p <- ggplot() +
#   geom_segment(data = sgmnt, aes(x, y, xend = xend, yend = yend), linetype = "dashed") +
#   ylab("Dependent variable") +
#   xlab("Independent variable") +
#   geom_point(data = model_data, aes(wtemp, wtemp, color = "Obs")) +
#   scale_color_manual(values = c("Obs" = "black"), name = "") +
#   #coord_cartesian(xlim = c(-5, 30), ylim = c(-5, 30)) +
#   theme_bw(base_size = 12)
# p
# ggsave(p,filename = "./app/www/example_linear_relationship.png", device = "png",height = 3.2, width = 4.2, units = "in")
# 
# p1 <- ggplot() +
#   geom_segment(data = sgmnt, aes(x, y, xend = xend, yend = yend), linetype = "dashed") +
#   ylab("Dependent variable") +
#   xlab("Independent variable") +
#   geom_point(data = model_data[c(1,250,500,750),], aes(wtemp, airt, color = "Obs")) +
#   scale_color_manual(values = c("Obs" = "black"), name = "") +
#   #coord_cartesian(xlim = c(-5, 30), ylim = c(-5, 30)) +
#   geom_smooth(data = model_data[c(1,250,500,750),], aes(wtemp, airt, color = "Mod"), method = "lm", formula = "y ~ x")   +
#   theme_bw(base_size = 12)
# p1
# ggsave(p1,filename = "./app/www/example_sparse_data.png", device = "png",height = 3.2, width = 4.2, units = "in")
