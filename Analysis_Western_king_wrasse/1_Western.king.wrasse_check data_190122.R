# BIOL4408 Marine Ecology field trip

###### Import western king wrasse data from gsheet ######
### Written by Tim Langlois 


##  What are we going to do with the data?----
##  1. Rough-Plot of the data to explore and look for outliers.
##  2. Calculate counts by stage.
##  3. Check and save the data



# Clear the workspace--
rm(list=ls())



# load librarys----
library(tidyr) #to tody data
library(dplyr) #to transform data
library(forcats) #to transform catagorical data
library(readr) #to write data
library(ggplot2) #to plot data
library(RCurl) #needed to download data from GitHub




# Set name for study--
study<-"western.king.wrasse"





# Set work directory----
# Set your own to match where the data sits on your computer

work.dir=("~/GitHub/BIOL4408/Analysis_Western_king_wrasse") #for Tim

work.dir=("~/workspace/BIOL4408/Analysis_Western_king_wrasse") #for ecocloud server
# or
#work.dir=("") #set this for your computer work directory


# Set sub-directories----
data.dir=paste(work.dir,"Data",sep="/")





# Read in the gsheet and check it----
setwd(data.dir)
dir()

# From local file
gsheet.dat <- read_csv("western.king.wrasse.gsheet.dat.csv") 
# OR
#Read from github
gsheet.dat<-read.csv(text=getURL("https://raw.githubusercontent.com/TimLanglois/BIOL4408/master/Analysis_Western_king_wrasse/Data/western.king.wrasse.gsheet.dat.csv"))



glimpse(gsheet.dat) #glimpse the data
#variable names, formats, head of the data


unique(gsheet.dat$status) #unique levels of status
unique(gsheet.dat$sanctuary) #unique levels of sanctuary
unique(gsheet.dat$site) #unique levels of site

unique(gsheet.dat$group.no) #this is not correct

unique(gsheet.dat$groupID) #this is important to make a unique transect number


unique(gsheet.dat$transect)
#looks consistent

unique(gsheet.dat$stage)
#does not look consistent - need to change j to J

unique(gsheet.dat$school)
#looks OK - the NA's are no WKW in a transect



# Read in the data from gsheet and make corrections and re-format----
dat <- gsheet.dat %>%
#recode the stage names
    dplyr::mutate(stage = fct_recode(stage,
                           "J" = "j"))%>%
  
  dplyr::mutate(number= ifelse(is.na(length.mm), 0, 1))%>%
  #remove the levels for the filtered facotrs
    droplevels()%>%
  
  # Make a unique group.id
  dplyr::mutate(group.id=paste(status,sanctuary,site,groupID,sep="."))%>%
  
  # Make a unique transect.id
  dplyr::mutate(transect.id=paste(status,sanctuary,site,groupID,transect,sep="."))%>%
  
  # Make a unique school.id
  dplyr::mutate(school.id=paste(site,transect,groupID,school,sep="."))%>%
  
  # Make a unique row.id
  dplyr::mutate(row.id=1:nrow(.))%>%
  
    glimpse()


# Basic plots to check out the data----

# Box plot (looks better with length data)----
ggplot(dat, aes(x=status, y=length.mm)) + 
  geom_boxplot(outlier.shape = NA, notch=FALSE, width=0.8)+
  geom_point(position = position_jitter(width = 0.1, h = 0),alpha = 1/4, size=1)+
  stat_summary(fun.y=mean, geom="point", shape=2, size=4)

ggplot(dat, aes(x=status, y=length.mm)) + 
  geom_boxplot(outlier.shape = NA, notch=FALSE, width=0.8)+
  geom_point(position = position_jitter(width = 0.1, h = 0),alpha = 1/4, size=1)+
  stat_summary(fun.y=mean, geom="point", shape=2, size=4)+
  facet_grid(stage~sanctuary)


  
  # Save the long data----
glimpse(dat)  
setwd(data.dir) #set the directory
  dir() #look in the directory
  write_csv(dat,paste(study,"csv",sep = "."))
  
  

#Calcualte extra data sets for:
  #Ratio of males to females per transect - only where males present and count of schools-----
# Use dat and make new variables of sum Male/Female/Juvenilles per transect and calculate ratios-----
  glimpse(dat)
  

  # Count of each stage--
wide.dat<-dat %>%
  group_by(sanctuary,status,group.id,transect.id,stage)%>%
  dplyr::summarise(count=sum(number))%>%
  spread(stage,count, fill = 0)%>%  #make wide
  glimpse()
  
  
# Count of schools--
schools.dat<-dat %>%
  filter(!is.na(length.mm))%>%
  group_by(sanctuary,status,group.id,transect.id)%>%
  dplyr::summarise(school.count=n_distinct(school.id))%>%
  glimpse()


# Summary data---
sum.dat<-wide.dat%>%
left_join(schools.dat)%>% #bring in the school data
  mutate(school.count = replace_na(school.count,0))%>% #add in zeros for schools
  select(-`<NA>`)%>% #dropping the NA stage for transect with no fish
  gather(key="metric",value="count",`F`, M,J,school.count)%>%
  # make a unique sample number
  ungroup()%>%
  dplyr::mutate(sample.no=1:nrow(.))%>%
  glimpse()

unique(sum.dat$metric)


# Ratio of F to M---
ratio.dat<-wide.dat%>%
  filter(M>0)%>%
  dplyr::mutate(FtoM = `F`/M)%>%
  select(-c(`<NA>`,M,J,`F`))%>%
  dplyr::rename(count=FtoM)%>%
  glimpse()




# Basic plots to check out the data----

# functions for summarising data in plots
se <- function(x) sd(x) / sqrt(length(x))
se.min <- function(x) (mean(x)) - se(x)
se.max <- function(x) (mean(x)) + se(x)


# Barplot By Status----
ggplot(sum.dat, aes(x=status, y=count,fill=status)) + 
  stat_summary(fun.y=mean, geom="bar") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1)+
facet_grid(metric~sanctuary, scales = "free")

ggplot(ratio.dat, aes(x=status, y=count,fill=status)) + 
  stat_summary(fun.y=mean, geom="bar") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1)+
  facet_grid(.~sanctuary, scales = "free")


# Write the data----
setwd(data.dir) #set the directory
dir() #look in the directory
write_csv(sum.dat,paste(study,"summary","csv",sep = "."))
write_csv(ratio.dat,paste(study,"ratio","csv",sep = "."))

