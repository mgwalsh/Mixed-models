# Mixed model analyses of global south cereal-yield gap data
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
ceyld$cereal_yield <- ceyld$cereal_prod / ceyld$cereal_area ## Mg/ha
ceyld <- ceyld[complete.cases(ceyld[ ,9]),]

# Maize yield calculations (Mg/ha)
mzyld <- merge(cereal_panel, area, by="id")
mzyld <- merge(mzyld, maize_prod, by="id")
mzyld$maize_yield <- mzyld$maize_prod / mzyld$maize_area ## Mg/ha maize yields
mzyld <- mzyld[complete.cases(mzyld[ ,11]),]

# Rice yield calculations (Mg/ha)
rcyld <- merge(cereal_panel, area, by="id")
rcyld <- merge(rcyld, rice_prod, by="id")
rcyld$rice_yield <- rcyld$rice_prod / rcyld$rice_area ## Mg/ha rice yields
rcyld <- rcyld[complete.cases(rcyld[ ,11]),]

# Wheat yield calculations (Mg/ha)
wtyld <- merge(cereal_panel, area, by="id")
wtyld <- merge(wtyld, wheat_prod, by="id")
wtyld$wheat_yield <- wtyld$wheat_prod / wtyld$wheat_area ## Mg/ha wheat yields
wtyld <- wtyld[complete.cases(wtyld[ ,11]),]

# Cereal productivity trends over time by country -------------------------
# Total cereal yield (Mg/ha) trends
cy.lme <- lmer(cereal_yield~I(year-1961)+(I(year-1961)|cc), ceyld) ## random intercept & slope model
summary(cy.lme)
plot(cereal_yield~fitted(cy.lme), ceyld)

# Maize yield (Mg/ha) trends
my.lme <- lmer(maize_yield~I(year-1961)+(I(year-1961)|cc), mzyld) ## random intercept & slope model
summary(my.lme)
plot(maize_yield~fitted(my.lme), mzyld)

# extract random effects
my.ran <- ranef(my.lme) ## extract random effects
my <- as.data.frame(rownames(my.ran$cc))
my$b0 <- my.ran$cc[,1]
my$b1 <- my.ran$cc[,2]
colnames(my) <- c("cc","b0","b1")

# Rice yield (Mg/ha) trends
ry.lme <- lmer(rice_yield~I(year-1961)+(I(year-1961)|cc), rcyld) ## random intercept & slope model
summary(ry.lme)
plot(rice_yield~fitted(ry.lme), rcyld)

# Wheat yield (Mg/ha) trends
wy.lme <- lmer(wheat_yield~I(year-1961)+(I(year-1961)|cc), wtyld) ## random intercept & slope model
summary(wt.lme)
plot(wheat_yield~fitted(wt.lme), wtyld)

# Maize, rice & wheat area trends over time by country --------------------
# Maize area (Mha) trends
ma.lme <- lmer(maize_area~I(year-1961)+(I(year-1961)|cc), mzyld) ## random intercept & slope model
summary(ma.lme)
plot(maize_area~fitted(ma.lme), mzyld)

# extract random effects
ma.ran <- ranef(ma.lme) ## extract random effects
ma <- as.data.frame(rownames(ma.ran$cc))
ma$b0 <- ma.ran$cc[,1]
ma$b1 <- ma.ran$cc[,2]
colnames(ma) <- c("cc","b0","b1")

# extract standard errors
ma.se <- se.coef(ma.lme) ## extract random effects
mae <- as.data.frame(rownames(mae.se$cc))
mae$b0 <- mae.se$cc[,1]
mae$b1 <- mae.se$cc[,2]
colnames(mae) <- c("cc","e0","e1")
