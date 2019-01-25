
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
# library(purrr)
# library(forcats) #to transform catagorical data
library(readr) #to write data
library(ggplot2) #to plot data
# if(!requireNamespace("devtools")) install.packages("devtools")
# devtools::install_github("dkahle/ggmap", ref = "tidyup",force = TRUE)
library(ggmap)
# library(tmaptools)
# library(broom)
library(rgeos)
require(rgdal)



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
coastline.dir=("~/Dropbox/RottnestLobsterPots_190123/Coastline")


# Read in polygon shapefiles------
setwd(polygon.dir)
dir()

ntr.shp <- readOGR(dsn = ".", layer = "sank_WGS84_z50_clean")


ntr<-shapefile(x="sank_WGS84_z50_clean.shp")%>%
  fortify()%>%
  dplyr::rename(X=long,Y=lat)%>%
  bind_cols(., UTM2deg(.$X, .$Y, zone = 50))%>%
  glimpse()




setwd(data.dir)
dir()
write_csv(ntr,paste("ntr",Sys.Date(),"csv",sep = "."))







# Read in coastline shapefiles------
# sudan <- aggregate(rbind(ssudan, nsudan))
# plot(sudan)


setwd(coastline.dir)
dir()

# rotto<-shapefile(x="Rotto.shp")
rotto <- readOGR(dsn = ".", layer = "Rotto")


rotto.poly<-as(rotto, "SpatialPolygons")

rotto.shp <- spTransform(rotto.poly,
                         CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

ntr.shp.new <- spTransform(ntr.shp,
                         CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

rotto.ntr <- raster::union(rotto.shp, ntr.shp.new)

# rotto.ntr <- rotto.shp


rotto.utm <- spTransform(rotto.ntr,
                      CRS("+proj=utm +zone=50 +datum=WGS84 +units=km +ellps=WGS84 +towgs84=0,0,0"))

rotto.expand.utm <- gBuffer(rotto.utm,width=1,quadsegs=4)

rotto.coastal <- spTransform(rotto.expand.utm,CRS("+proj=longlat +ellps=WGS84"))
plot(rotto.coastal,border="red")
plot(rotto.ntr,add=TRUE)

pts.all <- spsample(rotto.coastal,type="regular",n=1000)%>%
  glimpse()
plot(pts.all,pch=".")

pts.all<- spTransform(pts.all,
                      CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

rotto.shp.poly<-as(rotto.ntr, "SpatialPolygons")


onland <- sp::over(pts.all,rotto.shp.poly)
pts.wet <- pts.all[is.na(onland),]
plot(rotto.coastal,border="red")
plot(rotto.shp.poly,add=TRUE)
plot(pts.wet,add=TRUE,pch=".",col="blue")



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
glimpse(dat)
dir()
write_csv(dat,paste(study,Sys.Date(),"csv",sep = "."))



# Create raster of pots-----
library("KernSmooth")
glimpse(dat)

pot.2014 <- cbind(dat%>%
             filter(year==2014)%>%
             dplyr::select(lon),dat%>%
             filter(year==2014)%>%
             dplyr::select(lat))%>%
  bkde2D(bandwidth=c(0.001, 0.001))
pot.2014.raster = raster(list(x=pot.2014$x1,y=pot.2014$x2,z=pot.2014$fhat))
plot(pot.2014.raster)


pot.2015 <- cbind(dat%>%
                    filter(year==2015)%>%
                    dplyr::select(lon),dat%>%
                    filter(year==2015)%>%
                    dplyr::select(lat))%>%
  bkde2D(bandwidth=c(0.001, 0.001))
pot.2015.raster = raster(list(x=pot.2015$x1,y=pot.2015$x2,z=pot.2015$fhat))
plot(pot.2015.raster)
pot.2015.raster_res <- resample(pot.2015.raster, pot.2014.raster)

pot.2016 <- cbind(dat%>%
                    filter(year==2016)%>%
                    dplyr::select(lon),dat%>%
                    filter(year==2016)%>%
                    dplyr::select(lat))%>%
  bkde2D(bandwidth=c(0.001, 0.001))
pot.2016.raster = raster(list(x=pot.2016$x1,y=pot.2016$x2,z=pot.2016$fhat))
plot(pot.2016.raster)
pot.2016.raster_res <- resample(pot.2016.raster, pot.2014.raster)

pot.2017 <- cbind(dat%>%
                    filter(year==2017)%>%
                    dplyr::select(lon),dat%>%
                    filter(year==2017)%>%
                    dplyr::select(lat))%>%
  bkde2D(bandwidth=c(0.001, 0.001))
pot.2017.raster = raster(list(x=pot.2017$x1,y=pot.2017$x2,z=pot.2017$fhat))
plot(pot.2017.raster)
pot.2017.raster_res <- resample(pot.2017.raster, pot.2014.raster)

pot.2018 <- cbind(dat%>%
                    filter(year==2018)%>%
                    dplyr::select(lon),dat%>%
                    filter(year==2018)%>%
                    dplyr::select(lat))%>%
  bkde2D(bandwidth=c(0.001, 0.001))
pot.2018.raster = raster(list(x=pot.2018$x1,y=pot.2018$x2,z=pot.2018$fhat))
plot(pot.2018.raster)
pot.2018.raster_res <- resample(pot.2018.raster, pot.2014.raster)



# Extract vales from raster stack-----



rasStack = raster::stack(pot.2014.raster,pot.2015.raster_res,pot.2016.raster_res,pot.2017.raster_res,pot.2018.raster_res,quick=TRUE)


pts.pots<-dat%>% #I have used the pot locations
  select(lon,lat)%>%
  dplyr::rename(x1=lon,x2=lat)%>%
  filter(year==2014)
  glimpse()


ras.2014<-raster::extract(pot.2014.raster, dat%>%
                           filter(year==2014)%>%
                           select(lon,lat)%>%
                           dplyr::rename(x1=lon,x2=lat))
  ras.2014<-as.data.frame(ras.2014)%>%
  mutate(year=2014)%>%
  dplyr::rename(predict=ras.2014)%>%
  glimpse()




ras.2015=raster::extract(pot.2015.raster, dat%>%
                           filter(year==2015)%>%
                           select(lon,lat)%>%
                           dplyr::rename(x1=lon,x2=lat))
ras.2015<-as.data.frame(ras.2015)%>%
  mutate(year=2015)%>%
  dplyr::rename(predict=ras.2015)%>%
  glimpse()

ras.2016=raster::extract(pot.2016.raster, dat%>%
                           filter(year==2016)%>%
                           select(lon,lat)%>%
                           dplyr::rename(x1=lon,x2=lat))
ras.2016<-as.data.frame(ras.2016)%>%
  mutate(year=2016)%>%
  dplyr::rename(predict=ras.2016)%>%
  glimpse()

ras.2017=raster::extract(pot.2017.raster, dat%>%
                           filter(year==2017)%>%
                           select(lon,lat)%>%
                           dplyr::rename(x1=lon,x2=lat))
ras.2017<-as.data.frame(ras.2017)%>%
  mutate(year=2017)%>%
  dplyr::rename(predict=ras.2017)%>%
  glimpse()

ras.2018=raster::extract(pot.2018.raster, dat%>%
                           filter(year==2018)%>%
                           select(lon,lat)%>%
                           dplyr::rename(x1=lon,x2=lat))
ras.2018<-as.data.frame(ras.2018)%>%
  mutate(year=2018)%>%
  dplyr::rename(predict=ras.2018)%>%
  glimpse()


ras<-bind_rows(ras.2014,ras.2015,ras.2016,ras.2017,ras.2018)%>%
  glimpse()

dat.ras<-dat%>%
  bind_cols(ras)%>%
  glimpse()





