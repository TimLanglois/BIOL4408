
# BIOL4408 Marine Ecology field trip

###### Import lobster pot distribution from shapefiles ######
### Written by Tim Langlois 


##  What are we going to do with the data?----
##  1. Import data _Tim has done this in another script
##  2. Plot and check of the data to explore and look for outliers.
##  3. Save for analysis



# Clear the workspace--
rm(list=ls())



# load librarys----
# library(maptools)
# library(rdgal) 
library(raster)#to read shapefiles
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


# Functions-----
# Source the UTM2deg function
# From GitHub
source("https://gist.githubusercontent.com/TonyLadson/f37aab3e2ef517188a7f27166307c985/raw/0822970769bc90fcc28052a91b375399d665286e/UTM2deg.R")
# Or from folder on my github


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
setwd(polygon.dir)
dir()

ntr<-shapefile(x="sank_WGS84_z50_clean.shp")%>%
  fortify()%>%
  dplyr::rename(X=long,Y=lat)%>%
  bind_cols(., UTM2deg(.$X, .$Y, zone = 50))%>%
  glimpse()


setwd(data.dir)
dir()
write_csv(ntr,paste("ntr",Sys.Date(),"csv",sep = "."))



# Read in the points shapefiles------
setwd(shape.dir)

dir()

# lobster_2014.shp
lobster_2014<-shapefile(x="lobster_2014.shp")%>%
  data.frame()%>%
  mutate(year=2014)%>%
  glimpse()

# "lobster_2015G1.shp"
lobster_2015G1<-shapefile(x="lobster_2015G1.shp")%>%
  data.frame()%>%
  mutate(year=2015)%>%
  glimpse()

# "lobster_2015G2.shp"
lobster_2015G2<-shapefile(x="lobster_2015G2.shp")%>%
  data.frame()%>%
  mutate(year=2015)%>%
  glimpse()

# "lobster_2016.shp"
lobster_2016<-shapefile(x="lobster_2016.shp")%>%
  data.frame()%>%
  mutate(year=2016)%>%
  glimpse()

# "lobster_2017G1.shp"
lobster_2017G1<-shapefile(x="lobster_2017G1.shp")%>%
  data.frame()%>%
  mutate(year=2017)%>%
  glimpse()

# "lobster_2017G2.shp"
lobster_2017G2<-shapefile(x="lobster_2017G2.shp")%>%
  data.frame()%>%
  mutate(year=2017)%>%
  glimpse()

# "lobster_2018G1.shp"
lobster_2018G1<-shapefile(x="lobster_2018G1.shp")%>%
  data.frame()%>%
  mutate(year=2018)%>%
  glimpse()

# "lobster_2018G2.shp"
lobster_2018G2<-shapefile(x="lobster_2018G2.shp")%>%
  data.frame()%>%
  mutate(year=2018)%>%
  glimpse()


# Combine the data, convert UTM to degs, and make corrections to zone----
dat<-bind_rows(lobster_2014,lobster_2015G1,lobster_2015G2,lobster_2016,lobster_2017G1,lobster_2017G2,lobster_2018G1,lobster_2018G2)%>%
  # Add columns of lat and lon using the UTM2deg function
  bind_cols(., UTM2deg(.$X, .$Y, zone = 50))%>%
  #Change W to S for 2015 data to east of 115.50
  mutate(zone=ifelse(zone=="W"&lon>115.50,"S",.$zone))%>%
  mutate(zone=ifelse(is.na(zone),"W",.$zone))%>%
  glimpse()

# Write data-----
setwd(data.dir)
dir()
write_csv(dat,paste(study,Sys.Date(),"csv",sep = "."))
