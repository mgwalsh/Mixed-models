# Mixed model analyses of global south level cereal-yield gap data
# M.G. Walsh, October 2020

# Required packages
# install.packages(c("downloader","arm","dplyr")), dependencies=TRUE)
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

# Yield calculations ------------------------------------------------------
# Total cereal yield calculations (Mg/ha)
ceyld <- merge(cereal_panel, cereal_prod, by="id")
ceyld <- ceyld[complete.cases(ceyld[ ,7:8]),]
ceyld$cereal_yield <- ceyld$cereal_prod / ceyld$cereal_area ## Mg/ha

# Maize yield calculations (Mg/ha)
mzyld <- merge(cereal_panel, area, by="id")
mzyld <- merge(mzyld, maize_prod, by="id")
mzyld <- mzyld[complete.cases(mzyld[ ,7,10]),]
mzyld$maize_yield <- mzyld$maize_prod / mzyld$maize_area ## Mg/ha maize yields

# Rice yield calculations (Mg/ha)
rcyld <- merge(cereal_panel, area, by="id")
rcyld <- merge(rcyld, rice_prod, by="id")
rcyld <- mzyld[complete.cases(rcpan[ ,8,10]),]
rcyld$rice_yield <- rcyld$rice_prod / rcyld$rice_area ## Mg/ha rice yields

# Wheat yield calculations (Mg/ha)
wtyld <- merge(cereal_panel, area, by="id")
wtyld <- merge(wtyld, wheat_prod, by="id")
wtyld <- wtyld[complete.cases(wtyld[ ,9,10]),]
wtyld$wheat_yield <- wtyld$wheat_prod / wtyld$wheat_area ## Mg/ha wheat yields
