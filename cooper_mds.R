## Analysis of Cooper river data
# R Holt, CJ Brown 5 Nov 2015

rm(list = ls())

#
#Packages
#
library(dplyr)
library(vegan)
library(ggplot2)

#
# Data input
#

setwd('~/Databases/dryland_rivers')
list.files()
dat <- read.csv('cooper_data.csv', header = T, stringsAsFactors = F)

#
# Data prelim
#
dat$date <- as.Date(dat$date, "%d-%b-%y")

with(dat, table(date, waterhole))

head(dat)
names(dat)
N <- nrow(dat)
print(N)
nvar <- ncol(dat)
fsp <- 10 #First species column
sppnams <- names(dat)[fsp: nvar]
nspp <- length(sppnams)

#Define species data
sppdat <- dat[,fsp:nvar]

sppdat[is.na(sppdat)] <- 0

#Define meta data
datmeta <- dat[,1:(fsp-1)]


#
# Filter for temporal data
#
tsites <- c('Murken', 'Mayfield', 'Glen Murken', 'Shed WH ')
nsites <- length(tsites)
irow <- dat$waterhole %in% tsites

dat2 <- datmeta[irow,]
sppdat2 <- sppdat[irow,]
N2 <- nrow(dat2)
with(dat2, table(date, waterhole))
#
# MDS by sites and date
# 

xout <- metaMDS(sppdat2, distance = 'bray')
xout$points

# Build a df for plotting sites and dates

dfout <- data.frame(x = xout$points[,1], y = xout$points[,2], wh = dat2$waterhole, date = dat2$date)
with(dfout, table(date, wh))
dfout$unq <- factor(dfout$date)

#
# Plot MDS
#

ggplot(dfout, aes(x = x, y = y, colour = unq)) + 
geom_point(size =5) + facet_grid(. ~ wh)

#
# Look at species contributions
#

sort(xout$species[,1])
sort(xout$species[,2])


