# BIOL4408 Marine Ecology field trip

###### Import lobster density data from gsheet ######
### Written by Tim Langlois 


##  What are we going to do with the data?----
##  1. Import data
##  2. Check and save the data
##  3. Rough-Plot of the data to explore and look for outliers.



# Clear the workspace--
rm(list=ls())



# load librarys----
library(googlesheets) #to read gsheet
library(tidyr) #to tody data
library(dplyr) #to transform data
library(forcats) #to transform catagorical data
library(readr) #to write data
library(ggplot2) #to plot data




# Set name for study--
study<-"lobster.density"





# Set work directory----
# Set your own to match where the data sits on your computer

work.dir=("~/GitHub/BIOL4408/Analysis_Lobster_density") #for Tim

#work.dir=("") #set this for your computer work directory



# Set sub-directories----
data.dir=paste(work.dir,"Data",sep="/")





# Read in the data from gsheet and check it----
gs_ls() #list gsheets you have access to

dat <- gs_title("BIOL4408.lobster.density")%>% #select the gsheet
  gs_read_csv(ws = "lobster.density") #select the worksheet within the workbook


glimpse(dat) #glimpse the data
#variable names, formats, head of the data


unique(dat$status) #unique levels of status
unique(dat$sanctuary) #unique levels of sanctuary
unique(dat$site) #unique levels of site
# "eastsalmon" should be "Salmon Bay"


unique(dat$year) #unique levels of year 
# we are worried about 2017 - we should remove it

unique(dat$complexity)
#looks consistent

unique(dat$algal.cover)
#does not look consistent - suggest you only use for your year of data
unique(filter(dat,year=="2018")$algal.cover)


# Check what sites are missing between years---
table(dat$site,dat$year)
# "Armstrong Point","Longreach","Rocky Bay","Stark Bay" - are only sampled once and we should remove them


# Check how we have used site names - are they unique between levels of status? 
table(dat$site,dat$status)





# Read in the data from gsheet and make corrections and re-formating----
dat <- gs_title("BIOL4408.lobster.density")%>% 
    gs_read_csv(ws = "lobster.density") %>%
#recode the site names
    dplyr::mutate(site = fct_recode(site,
                           "Salmon Bay" = "eastsalmon"))%>%
    
  #filter out sites only done once
    filter(!site%in%c(
    "Armstrong Point",
    "Longreach",
    "Rocky Bay",
    "Stark Bay"))%>%
  
  #filter out suspicous year  
    filter(!year==2017)%>%
  
  #remove the levels for the filtered facotrs
    droplevels()%>%
  
  # Make a new unique Site namename
  dplyr::mutate(site.new=paste(site,status,sep="."))%>%
  
    glimpse()





# Use dat and make new varialbes for sum of legal and sub.legal-----
dat<-gs_title("BIOL4408.lobster.density")%>% 
  gs_read_csv(ws = "lobster.density") %>%
  dplyr::mutate(site = fct_recode(site,"Salmon Bay" = "eastsalmon"))%>%
  filter(!site%in%c(
    "Armstrong Point",
    "Longreach",
    "Rocky Bay",
    "Stark Bay"))%>%
  filter(!year==2017)%>%
  droplevels()%>%
  dplyr::mutate(site.new=paste(site,status,sep="."))%>%
  # make the legal sum
  dplyr::mutate(legal=(legal.unsized+x80+x85+x90+x95+x100+x105+x110+x115+x120+x125+x130+x135+x140+x145+x150))%>%
  # make the sub.legal sum
  dplyr::mutate(sub.legal=(sublegal.unsized+x25+x30+x35+x40+x45+x50+x55+x60+x65+x70+x75))%>%
  # make a unique sample number
  dplyr::mutate(sample.no=1:nrow(.))%>%
  # select the variables of interest
  select(c(sample.no,year,date,sanctuary,status,site.new,complexity,algal.cover,legal,sub.legal))%>%
  # # # make the data long again
  gather(key="size.class",value="count",legal, sub.legal)%>%
  glimpse()




# Write the long data----
setwd(data.dir) #set the directory
dir() #look in the directory
write.csv(dat,"dat.csv")

# Write dat using study name and system date
write_csv(dat,paste(study,Sys.Date(),"csv",sep = "."))





# Basic plots to check out the data----


# Point plot----
ggplot(data=dat, aes(x=status, y=count)) + 
  geom_point()


# Jittered point plot (great for checking data)----
ggplot(dat, aes(x=status, y=count)) + 
  geom_point(position = position_jitter(),alpha = 1/4) #alpha gives transparency


# Jittered point plot with one factor facetted----
ggplot(dat, aes(x=status, y=count)) + 
  geom_point(position = position_jitter(),alpha = 1/4)+
  facet_grid(size.class~.) #facet by factor


#jittered point plot with two factors faceted-----
ggplot(dat, aes(x=status, y=count, colour=status)) + 
  geom_point(position = position_jitter(width = 0.1, h = 0),alpha = 1/4)+
  facet_grid(size.class~year)


# Box plot (does not look very good!)----
ggplot(dat, aes(x=status, y=count)) + 
  geom_boxplot(outlier.shape = NA, notch=FALSE, width=0.8)+
  geom_point(position = position_jitter(width = 0.1, h = 0),alpha = 1/4, size=1)+
  stat_summary(fun.y=mean, geom="point", shape=2, size=4)+ #adds mean
  facet_grid(size.class~sanctuary)


