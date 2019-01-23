
# BIOL4408 Marine Ecology field trip

###### Import lobster pot distribution from shapefiles ######
### Written by Tim Langlois 


##  What are we going to do with the data?----
##  1. Import data - Tim has done tricky stuff in another script
##  2. Plot and check of the data to explore and look for outliers.
##  3. Save for analysis



# Clear the workspace--
rm(list=ls())



# load librarys----
# library(maptools)
# library(rdgal) 
# library(raster)#to read shapefiles
library(tidyr) #to tody data
library(dplyr) #to transform data
library(purrr)
library(forcats) #to transform catagorical data
library(readr) #to write data
library(ggplot2) #to plot data
# if(!requireNamespace("devtools")) install.packages("devtools")
# devtools::install_github("dkahle/ggmap", ref = "tidyup",force = TRUE)
library(ggmap)
library(tmaptools)
library(broom)



# Set name for study--
study<-"lobster.pots"


# Set work directory----
# Set your own to match where the data sits on your computer

work.dir=("~/GitHub/BIOL4408/Analysis_Lobster_pot distribution") #for Tim

#work.dir=("") #set this for your computer work directory


# Set sub-directories----
data.dir=paste(work.dir,"Data",sep="/")
plot.dir=paste(work.dir,"Data",sep="/")
shape.dir=("~/Dropbox/RottnestLobsterPots_190123/covariates")
polygon.dir=("~/Dropbox/RottnestLobsterPots_190123/Features")


# Read in polygon shapefiles------
setwd(data.dir)
dir()
ntr<-read_csv("ntr.2019-01-23.csv")




# Read in pot data----
setwd(data.dir)
dir()
dat<-read_csv("lobster.pots.2019-01-23.csv")



# Basic plots to check the data-----
setwd(plot.dir)
# Get map of Rottnest
bbox <- c(115.43,-32.04,  115.58,-31.975)
rotto.map<-(get_stamenmap(as.vector(bbox), zoom = 12, maptype="terrain"))


# Plot of pots

pot.map<-ggmap(rotto.map)+
  geom_point(aes(lon,lat),size=2,colour="Yellow",data=dat, alpha=0.25)+
  xlab('Longitude')+
  ylab('Lattitude')
pot.map


# Plot of pots with NTRs----

pot.map.ntr<-ggmap(rotto.map)+
  geom_point(aes(lon,lat,colour=zone),size=1,data=dat, alpha=0.25)+
  geom_polygon(data = fortify(ntr),
               aes(lon, lat, group = group),
               fill = "orange", colour = "red", alpha = 0.2) +
  xlab('Longitude')+
  ylab('Lattitude')+
  facet_wrap(~year,ncol=2)
pot.map.ntr


# Explore the pot distribution with distance from boundary----

glimpse(dat)

pot.hist<-ggplot(data=dat, aes(x=dst_sank))+
  geom_density(alpha=.5)+
  facet_grid( .~ zone)
pot.hist





