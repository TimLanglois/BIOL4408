# BIOL4408 Marine Ecology field trip

###### Import lobster density data from gsheet ######
### Written by Tim Langlois 


##  What are we going to do with the data?----
##  1. Check and save the data
##  2. Rough-Plot of the data to explore and look for outliers.



# Clear the workspace--
rm(list=ls())



# load librarys----
library(tidyr) #to tody data
library(dplyr) #to transform data
library(forcats) #to transform catagorical data
library(readr) #to write data
library(ggplot2) #to plot data
library(RCurl) #needed to download data from GitHub
library(here)

# Set working directory
data.dir<-here("Analysis_Lobster_density","Data")




# Set name for study--
study<-"lobster.density"




# Read in the data and check it----
here()

setwd(data.dir)
dir()

# Read from local file
gsheet.dat<-read_csv("lobster.density.gsheet.csv")

str(gsheet.dat)
# OR
#Read from github
gsheet.dat<-read.csv(text=getURL("https://raw.githubusercontent.com/TimLanglois/BIOL4408/master/Analysis_Lobster_density/Data/lobster.density.gsheet.csv"))

  
  

glimpse(gsheet.dat) #glimpse the data
#variable names, formats, head of the data


unique(gsheet.dat$status) #unique levels of status
unique(gsheet.dat$sanctuary) #unique levels of sanctuary
unique(gsheet.dat$site) #unique levels of site
# "eastsalmon" should be "Salmon Bay"


unique(gsheet.dat$year) #unique levels of year 
# we are worried about 2017 - we should remove it

unique(gsheet.dat$complexity)
#looks consistent

unique(gsheet.dat$algal.cover)
#does not look consistent - suggest you only use for your year of data
unique(filter(gsheet.dat,year=="2019")$algal.cover)


# Check what sites are missing between years---
table(gsheet.dat$site,gsheet.dat$year)
# "Armstrong Point","Longreach","Rocky Bay","Stark Bay" - are only sampled once and we should remove them


# Check how we have used site names - are they unique between levels of status? 
table(gsheet.dat$year,gsheet.dat$status)


gsheet.dat%>%
  filter()


# Make corrections and re-formating----
dat<-gsheet.dat %>%
  dplyr::mutate(status = fct_recode(status,
                                    "No-take" = "No-Take"))%>%
  dplyr::mutate(sanctuary = fct_recode(sanctuary,
                                    "Parker Point" = "Parker_Pt",
                                    "Green Island" = "Green_Island",
                                    "Armstrong Bay" = "Armstrong"))%>%
  
#recode the site names
  
    dplyr::mutate(site = fct_recode(site,
                           "Salmon Bay" = "eastsalmon",
                           "City of York" = "City of York Bay",
                           "Ricey Beach" = "Ricey_Bay",
                           "Salmon Bay" = "East_Salmon",
                           "Little Salmon" = "Little_Salmon",
                           "Parker Point" = "Poc_Reef",
                           "Green Island" = "Green_Island",
                           "Parakeet Bay" = "Parakeet_Bay",
                           "Salmon Bay" = "West_Salmon_Bay",
                           "Little Armstrong" = "Little_Armstrong_Bay",
                           "Geordie Bay" = "Geordie_Bay",
                           "Mary Cove" = "Mary_Cove",
                           "Strickland Bay" = "East_Strickland"))%>%
    
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



unique(dat$sanctuary) #unique levels of sanctuary

unique(dat$site) #unique levels of site

unique(dat$status) #unique levels of status

str(dat)

glimpse(gsheet.dat)


# Use dat and make new varialbes for sum of legal and sub.legal-----
dat<-gsheet.dat%>% 
  dplyr::mutate(status = fct_recode(status,
                                    "No-take" = "No-Take"))%>%
  dplyr::mutate(sanctuary = fct_recode(sanctuary,
                                       "Parker Point" = "Parker_Pt",
                                       "Green Island" = "Green_Island",
                                       "Armstrong Bay" = "Armstrong"))%>%
  dplyr::mutate(site = fct_recode(site,
                                  "Salmon Bay" = "eastsalmon",
                                  "City of York" = "City of York Bay",
                                  "Ricey Beach" = "Ricey_Bay",
                                  "Salmon Bay" = "East_Salmon",
                                  "Little Salmon" = "Little_Salmon",
                                  "Parker Point" = "Poc_Reef",
                                  "Green Island" = "Green_Island",
                                  "Parakeet Bay" = "Parakeet_Bay",
                                  "Salmon Bay" = "West_Salmon_Bay",
                                  "Little Armstrong" = "Little_Armstrong_Bay",
                                  "Geordie Bay" = "Geordie_Bay",
                                  "Mary Cove" = "Mary_Cove",
                                  "Strickland Bay" = "East_Strickland"))%>%
  filter(!site%in%c(
    "Armstrong Point",
    # "Longreach",
    "Rocky Bay",
    "Stark Bay"))%>%
  filter(!year==2017)%>%
  droplevels()%>%
  # replace(is.na(.), 0)%>%
  
  mutate_at(vars(starts_with("x")), funs(ifelse(is.na(.),0,.)))%>%
  mutate_at(vars(starts_with("unsi")), funs(ifelse(is.na(.),0,.)))%>%
  mutate_at(vars(starts_with("legal")), funs(ifelse(is.na(.),0,.)))%>%
  mutate_at(vars(starts_with("sub")), funs(ifelse(is.na(.),0,.)))%>%
  
  # mutate_at(vars(ends_with("delay")), funs(abs, round))
  dplyr::mutate(site.new=paste(site,status,sep="."))%>%
  # make the legal sum
  dplyr::mutate(legal=(legal.unsized+x80+x85+x90+x95+x100+x105+x110+x115+x120+x125+x130+x135+x140+x145+x150))%>%
  # make the sub.legal sum
  dplyr::mutate(sub.legal=(sublegal.unsized+x25+x30+x35+x40+x45+x50+x55+x60+x65+x70+x75))%>%
  dplyr::mutate(all=(legal+sub.legal+unsized))%>%
  # make a unique sample number
  dplyr::mutate(sample.no=1:nrow(.))%>%
  # select the variables of interest
  select(c(sample.no,year,date,sanctuary,status,site.new,complexity,algal.cover,legal,sub.legal,all))%>%
  # # # make the data long again
  gather(key="size.class",value="count",legal, sub.legal,all)%>%
  glimpse()




# Write the long data----

setwd(data.dir) #set the directory
dir() #look in the directory
write.csv(dat,"dat.csv")

# Write dat using study name and system date
write_csv(dat,paste(study,"csv",sep = "."))





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
  facet_grid(.~year) #facet by factor


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


