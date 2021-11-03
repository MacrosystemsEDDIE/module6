# Load required libraries
suppressPackageStartupMessages(library(shiny, quietly = TRUE))
suppressPackageStartupMessages(library(DT, quietly = TRUE))
suppressPackageStartupMessages(library(sf, quietly = TRUE))
suppressPackageStartupMessages(library(leaflet, quietly = TRUE))
suppressPackageStartupMessages(library(plotly, quietly = TRUE))
suppressPackageStartupMessages(library(ggpubr, quietly = TRUE))

# Colors for plots
scale_colour_discrete <- ggthemes::scale_colour_colorblind
scale_fill_discrete <- ggthemes::scale_fill_colorblind
cols <- RColorBrewer::brewer.pal(8, "Dark2")
l.cols <- RColorBrewer::brewer.pal(8, "Set2")[-c(1, 2)]

# Functions required
source("R/download_phenocam.R")
source("R/get_html.R")
source("R/NP_model.R")
source("R/create_np_inputs.R")
source("R/textAreaInput2.R")

# Load in sp format with coordinates
neon_sites <- readRDS("data/neon_sites.rds")
neon_sites <- neon_sites[which(neon_sites$siteID %in% c("CRAM", "BARC", "PRPO", "LIRO", "PRLA")), ]
neon_sites$uid <- paste0("M", seq_len(nrow(neon_sites)))

#Load in the dataframe
neon_sites_df <- read.csv("data/neon_sites.csv")
neon_sites_df$long <- round(neon_sites_df$long, 3)
neon_sites_df$lat <- round(neon_sites_df$lat, 3)
neon_sites_df <- neon_sites_df[which(neon_sites_df$siteID %in% c("CRAM", "BARC", "PRPO", "LIRO", "PRLA")), ]
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
stats <- list("Minimum" = "Min.", "1st Quartile" = "1st Qu.", "Median" = "Median", "Mean" = "Mean", "3rd Quartile" = "3rd Qu.", "Maximum" = "Max.", "Standard Deviation" = "sd")
mod_choices <- c("Negative", "No change", "Positive")
# Sorting variables
state_vars <- c("Phytoplankton", "Nitrogen")
process_vars <- c("Mortality", "Uptake")

# Parameters for NP model
parms <- c(
  maxUptake = 1.0, #day-1
  kspar=120, #uEinst m-2 s-1
  ksdin=0.5, #mmol m-3
  maxGrazing=1.0, # day-1
  ksphyto=1, #mmol N m-3
  pFaeces=0.3, #unitless
  mortalityRate=0.4, #(mmmolN m-3)-1 day-1
  excretionRate=0.1, #day-1
  mineralizationRate=0.1, #day-1
  Chl_Nratio = 1, #mg chl (mmolN)-1
  Q10 = 2,  #unitless
  refTEMP = 20 # Reference temperature for q10
)

calib_model_png <- gsub("www/", "", list.files("www/calib_model/", full.names = TRUE))

# Initial conditions for NP
yini <- c(
  PHYTO = 2, #mmolN m-3
  DIN = 9) #mmolN m-3

mytheme <- theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
                 axis.text.x=element_text(size=18, colour='black'), axis.text.y=element_text(size=18, colour='black'),
                 axis.title.x=element_text(size=18), axis.title.y=element_text(size=18),
                 strip.text.x = element_text(size=14), strip.text.y = element_text(size=14),
                 panel.background = element_rect(fill = NA, color = "black"),
                 panel.grid.major = element_line(colour = "gray"),
                 legend.text=element_text(size=16),
                 legend.title = element_text(size = 20))
# Linear regression variables ----
lin_reg_vars <- read.csv("data/multiple_linear_regression_variables.csv",
                         fileEncoding = "UTF-8-BOM")

# Forecast Date
fc_date <- "2020-09-25"

# Sampling frequency
samp_freq <- c("Monthly", "Fortnightly", "Weekly", "Daily")

# end
