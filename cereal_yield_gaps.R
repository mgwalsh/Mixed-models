# Meta-analyses of "global south" cereal yield and area data
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
# download("", "Cereal_yield_gap.zip", mode = "wb") ## post later
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
# Cereal yield calculations (Mg/ha)
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
cy.lme <- lmer(log(cereal_yield+1)~I(year-2020)+(I(year-2020)|cc), ceyld) ## random intercept & slope model
summary(cy.lme)
plot(cereal_yield~exp(fitted(cy.lme))-1, ceyld) ## overall model fit

# Maize yield (Mg/ha) trends
my.lme <- lmer(log(maize_yield+1)~I(year-2020)+(I(year-2020)|cc), mzyld) ## random intercept & slope model
summary(my.lme)
plot(maize_yield~exp(fitted(my.lme))-1, mzyld)

# extract random effects
my.ran <- ranef(my.lme) ## extract random effects
my <- as.data.frame(rownames(my.ran$cc))
my$b0 <- my.ran$cc[,1]
my$b1 <- my.ran$cc[,2]
colnames(my) <- c("cc","b0","b1")

# extract standard errors
mye.se <- se.coef(my.lme) ## extract random effects
mye <- as.data.frame(rownames(mye.se$cc))
mye$se0 <- mye.se$cc[,1]
mye$se1 <- mye.se$cc[,2]
colnames(mye) <- c("cc","se0","se1")
maize_yield <- merge(my, mye, by="cc")

# Rice yield (Mg/ha) trends
ry.lme <- lmer(log(rice_yield+1)~I(year-2020)+(I(year-2020)|cc), rcyld) ## random intercept & slope model
summary(ry.lme)
plot(rice_yield~exp(fitted(ry.lme))-1, rcyld)

# Wheat yield (Mg/ha) trends
wy.lme <- lmer(log(wheat_yield+1)~I(year-2020)+(I(year-2020)|cc), wtyld) ## random intercept & slope model
summary(wt.lme)
plot(wheat_yield~exp(fitted(wy.lme))-1, wtyld)

# Maize, rice & wheat area trends over time by country --------------------
# Maize area (ha) trends
ma.lme <- lmer(log(maize_area+1)~I(year-2020)+(I(year-2020)|cc), mzyld) ## random intercept & slope model
summary(ma.lme)
plot(maize_area~exp(fitted(ma.lme))-1, mzyld)

# extract random effects
ma.ran <- ranef(ma.lme) ## extract random effects
ma <- as.data.frame(rownames(ma.ran$cc))
ma$b0 <- ma.ran$cc[,1]
ma$b1 <- ma.ran$cc[,2]
colnames(ma) <- c("cc","b0","b1")

# extract standard errors
mae.se <- se.coef(ma.lme) ## extract random effects
mae <- as.data.frame(rownames(mae.se$cc))
mae$se0 <- mae.se$cc[,1]
mae$se1 <- mae.se$cc[,2]
colnames(mae) <- c("cc","se0","se1")
maize_area <- merge(ma, mae, by="cc")

# write country-level output dataframe
dir.create("Results", showWarnings=F)
