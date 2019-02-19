
# BIOL4408 Marine Ecology field trip

###### Import lobster pot distribution and plot ######
### Written by Tim Langlois 


##  What are we going to do with the data?----
##  1. Import data - Tim has done tricky stuff in another script
##  2. Plot and describe the data.



# Clear the workspace--
rm(list=ls())





# load librarys----
library(tidyr) #to tidy data
library(dplyr) #to transform data
library(readr) #to write data
library(ggplot2) #to plot data
# if(!requireNamespace("devtools")) install.packages("devtools")
# devtools::install_github("dkahle/ggmap", ref = "tidyup",force = TRUE)
library(ggmap)
library(magrittr)




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
ntr<-read_csv("ntr.2019-01-23.csv")%>%
  glimpse()


# Read in pot data----
dat<-read_csv("lobster.pots.2019-01-23.csv")%>%
  glimpse()



# Basic plots to check the data-----
setwd(plot.dir)
# Get map of Rottnest
bbox <- c(115.435,-32.04,  115.57,-31.975)
rotto.map<-(get_stamenmap(as.vector(bbox), zoom = 14, maptype="terrain"))


ggmap(rotto.map)

# Plot of pots

ggmap(rotto.map)+
  # geom_point(aes(lon,lat),size=2,colour="Yellow",data=dat, alpha=0.25)+
  xlab('Longitude')+
  ylab('Lattitude')


# Plot of pots with NTRs----

ggmap(rotto.map)+
  geom_point(aes(lon,lat,colour="red"),size=1,data=dat, alpha=0.25,show.legend = F)+
  # geom_polygon(data = fortify(ntr), aes(lon, lat, group = group), colour = "green", fill="orange",alpha = 0.5) +
  
  geom_polygon(data = fortify(ntr%>%filter(lon<115.48)), aes(lon, lat, group = group), colour = "green", fill="orange",alpha = 0.5) +
  geom_polygon(data = fortify(ntr%>%filter(lon>115.48)), aes(lon, lat, group = group), colour = "green", fill="green",alpha = 0.25) +
  
  # xlim(115.43,115.57)+
  # xlab(' ')+
  # ylab(' ')+
  theme(axis.text=element_blank(),
        
        axis.title=element_blank(),
        axis.ticks=element_blank())
# +
#   facet_wrap(~year,ncol=2)


# Save the plot----
setwd(plot.dir)
dir()

ggsave(file="lobster.pots.png", width = 21, height = 10,units = "cm")
ggsave(file="rotto.png", width = 21, height = 10,units = "cm")
ggsave(file="rotto.nts.png", width = 21, height = 10,units = "cm")





# Explore the pot distribution within different zones of the island with different metrics----

glimpse(dat)


# Plot with distance to NTR boundary-
labels <- c(N = "North", S = "South")

ggplot(data=dat%>%filter(!zone=="W"), aes(x=dst_sank))+
  geom_density(alpha=.5)+
  facet_grid( .~ zone,labeller=labeller(zone = labels))+
  theme_classic()+
  xlab('Distance to sanctuary (m)')+
  ylab('Density')+
  Theme1

  # Save the plot----
  setwd(plot.dir)
  dir()
  
  ggsave(file="lobster.distance.png", width = 20, height = 10,units = "cm")
  


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








# Set Themes for plotting ----
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.background = element_rect(fill="white"),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=10),
    legend.title = element_blank(),
    legend.position = "top",
    legend.direction="horizontal",
    text=element_text(size=10),
    # strip.text.y = element_text(size = 10,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=12),
    axis.title.y=element_text(vjust=0.6, angle=90, size=12),
    axis.text.x=element_text(size=10,angle = 0),
    axis.text.y=element_blank(),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.text.y = element_text(size = 10,angle = 0),
    strip.text.x = element_text(size = 12,angle = 0),
    strip.background = element_blank())





