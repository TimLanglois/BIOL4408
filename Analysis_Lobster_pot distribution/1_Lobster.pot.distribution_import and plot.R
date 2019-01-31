
# BIOL4408 Marine Ecology field trip

###### Import lobster pot distribution and plot ######
### Written by Tim Langlois 


##  What are we going to do with the data?----
##  1. Import data - Tim has done tricky stuff in another script
##  2. Plot and describe the data.



# Clear the workspace--
rm(list=ls())



if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup",force = TRUE)


# load librarys----
library(tidyr) #to tidy data
library(dplyr) #to transform data
library(readr) #to write data
library(ggplot2) #to plot data
# install.packages("ggmap") #Run once then # out

library(ggmap)




# Set name for study--
study<-"lobster.pots"


# Set work directory----
# Set your own to match where the data sits on your computer

work.dir=("~/GitHub/BIOL4408/Analysis_Lobster_pot distribution") #for Tim's desktop

work.dir=("~/workspace/BIOL4408/Analysis_Lobster_pot distribution") #for ecocloud server
# or
#work.dir=("") #set this for your computer work directory




# Set sub-directories----
data.dir=paste(work.dir,"Data",sep="/")
plot.dir=paste(work.dir,"Plots",sep="/")


# Read in polygons of NTR------
setwd(data.dir)
dir()
ntr<-read_csv("ntr.2019-01-23.csv")


# Read in pot data----
dat<-read_csv("lobster.pots.2019-01-23.csv")%>%
  glimpse()



# Basic plots to check the data-----
setwd(plot.dir)
# Get map of Rottnest
bbox <- c(115.43,-32.04,  115.58,-31.975)
rotto.map<-(get_stamenmap(as.vector(bbox), zoom = 12, maptype="terrain"))


# Plot of pots

ggmap(rotto.map)+
  geom_point(aes(lon,lat),size=2,colour="Yellow",data=dat, alpha=0.25)+
  xlab('Longitude')+
  ylab('Lattitude')


# Plot of pots with NTRs----

ggmap(rotto.map)+
  geom_point(aes(lon,lat,colour=zone),size=1,data=dat, alpha=0.25)+
  geom_polygon(data = fortify(ntr),
               aes(lon, lat, group = group),
               fill = "orange", colour = "red", alpha = 0.2) +
  xlab('Longitude')+
  ylab('Lattitude')+
  facet_wrap(~year,ncol=2)




# Explore the pot distribution within different zones of the island with different metrics----

glimpse(dat)


# Plot with distance to NTR boundary-
ggplot(data=dat, aes(x=dst_sank))+
  geom_density(alpha=.5)+
  facet_grid( .~ zone)





# Plot with depth-
ggplot(data=dat, aes(x=depth))+
  geom_density(alpha=.5)+
  facet_grid( .~ zone)





# Plot with distance to jetties-
ggplot(data=dat, aes(x=dst_jettie))+
  geom_density(alpha=.5)+
  facet_grid( .~ zone)





# Plot with distance to moorings
ggplot(data=dat, aes(x=dst_moorin))+
  geom_density(alpha=.5)+
  facet_grid( .~ zone)





# Plot with distance to coast
pot.cst.hist<-ggplot(data=dat, aes(x=dst_cst))+
  geom_density(alpha=.5)+
  facet_grid( .~ zone)
pot.cst.hist








