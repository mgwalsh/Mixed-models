# Mixed model analyses of "global south" national prioritization data
# M.G. Walsh, November 2020

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
cereal_prod <- read.table("cereal_prod.csv", header = T, sep = ",") ## area and yield report for cereals
area <- read.table("maize_rice_wheat_area.csv", header = T, sep = ",") ## maize, rice & wheat area panel (ha)
maize_prod <- read.table("maize_prod.csv", header = T, sep = ",") ## total maize production (Mg) 
rice_prod <- read.table("rice_prod.csv", header = T, sep = ",") ## total rice production estimates (Mg)
wheat_prod <- read.table("wheat_prod.csv", header = T, sep = ",") ## total wheat production estimates (Mg)
fert_consumption <- read.table("fert_consumption.csv", header = T, sep = ",") ## fertilizer consumption (kg/ha for arable land)

# Yield calculations ------------------------------------------------------
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

# Cereal yield trends over time by country --------------------------------
# Maize yield (Mg/ha) trends by country
my.lme <- lmer(log(maize_yield+1)~I(year-2020)+(I(year-2020)|cc), mzyld) ## random intercept & trend model
summary(my.lme)

# diagnostic plot
par(pty="s")
plot(maize_yield~exp(fitted(my.lme))-1, xlim=c(0,15), ylim=c(0,15), xlab="Expected maize yield (Mg/ha)",
     ylab="Reported maize yield (Mg/ha)", cex.lab=1.3, mzyld)
abline(c(0,1), col="red", lwd=2)

# extract country-level coeficients
my.coef <- coef(my.lme) ## extract random effects
my <- as.data.frame(rownames(my.coef$cc))
my$y0 <- my.coef$cc[,1]
my$yt <- my.coef$cc[,2]
colnames(my) <- c("cc","y0","yt")

# extract country-level standard errors
mye.se <- se.coef(my.lme) ## extract random effects
mye <- as.data.frame(rownames(mye.se$cc))
mye$se0 <- mye.se$cc[,1]
mye$se1 <- mye.se$cc[,2]
colnames(mye) <- c("cc","sey0","sey1")
maize_yield <- merge(my, mye, by="cc")

# Rice yield (Mg/ha) trends
ry.lme <- lmer(log(rice_yield+1)~I(year-2020)+(I(year-2020)|cc), rcyld) ## random intercept & slope model
summary(ry.lme)

# diagnostic plot
par(pty="s")
plot(rice_yield~exp(fitted(ry.lme))-1, xlim=c(0,12), ylim=c(0,12), xlab="Expected rice yield (Mg/ha)",
     ylab="Reported rice yield (Mg/ha)", cex.lab=1.3, rcyld)
abline(c(0,1), col="red", lwd=2)

# extract country-level coeficients
ry.coef <- coef(ry.lme) ## extract random effects
ry <- as.data.frame(rownames(ry.coef$cc))
ry$y0 <- ry.coef$cc[,1]
ry$yt <- ry.coef$cc[,2]
colnames(ry) <- c("cc","ry0","ryt")

# extract country-level standard errors
rye.se <- se.coef(ry.lme) ## extract random effects
rye <- as.data.frame(rownames(rye.se$cc))
rye$se0 <- rye.se$cc[,1]
rye$se1 <- rye.se$cc[,2]
colnames(rye) <- c("cc","rsey0","rsey1")
rice_yield <- merge(ry, rye, by="cc")

# Wheat yield (Mg/ha) trends
wy.lme <- lmer(log(wheat_yield+1)~I(year-2020)+(I(year-2020)|cc), wtyld) ## random intercept & slope model
summary(wy.lme)

# diagnostic plot
par(pty="s")
plot(wheat_yield~exp(fitted(wy.lme))-1, xlim=c(0,10), ylim=c(0,10), xlab="Expected wheat yield (Mg/ha)",
     ylab="Reported wheat yield (Mg/ha)", cex.lab=1.3, wtyld)
abline(c(0,1), col="red", lwd=2)

# extract country-level coeficients
wy.coef <- coef(wy.lme) ## extract random effects
wy <- as.data.frame(rownames(wy.coef$cc))
wy$y0 <- wy.coef$cc[,1]
wy$yt <- wy.coef$cc[,2]
colnames(wy) <- c("cc","y0","yt")

# extract country-level standard errors
wye.se <- se.coef(wy.lme) ## extract random effects
wye <- as.data.frame(rownames(wye.se$cc))
wye$se0 <- wye.se$cc[,1]
wye$se1 <- wye.se$cc[,2]
colnames(wye) <- c("cc","sey0","sey1")
wheat_yield <- merge(wy, wye, by="cc")

# Maize, rice & wheat area trends over time by country --------------------
# Maize cropland area trends
ma.glmer <- glmer(maize_area~I(year-2020)+(I(year-2020)|cc), family=poisson, mzyld) ## random intercept & slope model
summary(ma.glmer)
plot(maize_area~fitted(ma.glmer), mzyld)

# extract country-level coeficients
ma.coef <- coef(ma.glmer) ## extract random effects
ma <- as.data.frame(rownames(ma.coef$cc))
ma$b0 <- ma.coef$cc[,1]
ma$b1 <- ma.coef$cc[,2]
colnames(ma) <- c("cc","a0","at")

# extract standard errors
mae.se <- se.coef(ma.glmer) ## extract random effects
mae <- as.data.frame(rownames(mae.se$cc))
mae$se0 <- mae.se$cc[,1]
mae$se1 <- mae.se$cc[,2]
colnames(mae) <- c("cc","sea0","sea1")
maize_area <- merge(ma, mae, by="cc")

# Plots -------------------------------------------------------------------
# Maize yields and areas
maize <- merge(maize_yield, maize_area, by="cc")
par(pty="s")

# historical trends
plot(yt~at, xlab="Maize area growth rate", ylab="Maize yield growth rate", cex.lab=1.3,
     pch=3, xlim=c(-0.15,0.15), ylim=c(-0.02,0.07), maize) 
abline(h=0.011, v=0.012, lty=2)
text(maize$at, maize$yt, maize$cc, cex=0.65, pos=3, col="red") ## country codes

# 2020 area and yield estimates
maize$y20 <- exp(maize$y0)-1
maize$a20 <- exp(maize$a0)/1000000
plot(y20~a20, xlab="Maize area 2020 (Mha)", ylab="Maize yield 2020 (Mg/ha)", cex.lab=1.3,
     pch=3, xlim=c(0,40), ylim=c(0,15), maize)
text(maize$a20, maize$y20, maize$cc, cex=0.65, pos=3, col="red") ## country codes

# write country-level output dataframes
dir.create("Results", showWarnings=F)


