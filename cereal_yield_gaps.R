# Mixed model analyses of national level cereal-yield gap data
# M.G. Walsh, October 2020

# Required packages
# install.packages(c("downloader","arm")), dependencies=TRUE)
suppressPackageStartupMessages({
  require(downloader)
  require(arm)
  require(dplyr)
})

# Data setup --------------------------------------------------------------
# Create a data folder in your current working directory
dir.create("Yield_gap", showWarnings=F)
setwd("./Yield_gap")

# download data
download("", "Cereal_yield_gap.zip", mode = "wb")
unzip("Cereal_yield_gap.zip", overwrite = T)
cereal_panel <- read.table("cereal_panel.csv", header = T, sep = ",") ## global south data only
hdi <- read.table("hdi.csv", header = T, sep = ",") ## human development index
arable_perc <- read.table("arable_perc.csv", header = T, sep = ",") ##  share of arable area (%)
ag_employ <- read.table("ag_employ.csv", header = T, sep = ",") ## share of ag employed (%)
area <- read.table("maize_rice_wheat_area.csv", header = T, sep = ",") ## maize, rice, wheat area harvested
cereal_prod <- read.table("cereal_prod.csv", header = T, sep = ",") ## area and yield report for cereals
maize_prod <- read.table("maize_prod.csv", header = T, sep = ",") ## total maize production (Mg) 
rice_prod <- read.table("rice_prod.csv", header = T, sep = ",") ## total rice production estimates (Mg)
wheat_prod <- read.table("wheat_prod.csv", header = T, sep = ",") ## total wheat production estimates (Mg)
fert_consumption <- read.table("fert_consumption.csv", header = T, sep = ",") ## fertilizer consumption (kg/ha for arable land)

# Cereal yield calculation
cepan <- merge(cereal_panel, cereal_prod, by="id")
cepan <- cepan[complete.cases(cepan[ ,7:8]),]
cepan$cereal_yield <- cepan$cereal_prod / cepan$cereal_area ## Mg / ha
