# BIOL4408 Marine Ecology field trip

###### Import western king wrasse data from gsheet ######
### Written by Tim Langlois 


##  What are we going to do with the data?----
##  1. Import data
##  2. Rough-Plot of the data to explore and look for outliers.
##  3. Calculate counts by stage.
##  4. Check and save the data



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
study<-"wester.king.wrasse"





# Set work directory----
# Set your own to match where the data sits on your computer

work.dir=("~/GitHub/BIOL4408/Analysis_Western_king_wrasse") #for Tim

#work.dir=("") #set this for your computer work directory



# Set sub-directories----
data.dir=paste(work.dir,"Data",sep="/")





# Read in the data from gsheet and check it----
gs_ls() #list gsheets you have access to

dat <- gs_title("BIOL4408.western.king.wrasse")%>% #select the gsheet
  gs_read_csv(ws = "western.king.wrasse") #select the worksheet within the workbook


glimpse(dat) #glimpse the data
#variable names, formats, head of the data


unique(dat$status) #unique levels of status
unique(dat$sanctuary) #unique levels of sanctuary
unique(dat$site) #unique levels of site

unique(dat$group.no) #this is not correct

unique(dat$groupID) #this is important to make a unique transect number


unique(dat$transect)
#looks consistent

unique(dat$stage)
#does not look consistent - need to change j to J

unique(dat$school)
#looks OK



# Read in the data from gsheet and make corrections and re-format----
dat <- gs_title("BIOL4408.western.king.wrasse")%>% 
  gs_read_csv(ws = "western.king.wrasse") %>%
#recode the stage names
    dplyr::mutate(stage = fct_recode(stage,
                           "J" = "j"))%>%
  
  dplyr::mutate(number= ifelse(is.na(length.mm), 0, 1))%>%
  #remove the levels for the filtered facotrs
    droplevels()%>%
  
  # Make a unique transect.id
  dplyr::mutate(transect.id=paste(groupID,transect,sep="."))%>%
  
  # Make a unique school.id
  dplyr::mutate(school.id=paste(site,transect,groupID,school,sep="."))%>%
  
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
  setwd(data.dir) #set the directory
  dir() #look in the directory
  write_csv(dat,paste(study,Sys.Date(),"csv",sep = "."))
  
  

#Ratio of males to females per transect.
  
  

# Use dat and make new variables of sum Male/Female/Juvenilles per transect and calculate ratios-----
  glimpse(dat)
  
# Count of schools--
  dat.schools<-dat %>%
    filter(!is.na(length.mm))%>%
    group_by(sanctuary,status,site,transect.id)%>%
    dplyr::summarise(school.count=n_distinct(school.id))%>%
    glimpse()
  
  # Count of each stage--
dat.summary<-dat %>%
  group_by(sanctuary,status,site,transect.id,stage)%>%
  dplyr::summarise(count=sum(number))%>%
  spread(stage,count, fill = 0)%>%  #make wide
  dplyr::mutate(MtoF = M/`F`)%>%
  left_join(dat.schools)%>% #bring in the school data 
  mutate(school.count = replace_na(school.count,0))%>% #add in zeros for schools
  select(-`<NA>`)%>% #dropping the NA stage for transect with no fish
  gather(key="metric",value="count",`F`, M,J,MtoF,school.count)%>%
  # make a unique sample number
  ungroup()%>%
  dplyr::mutate(sample.no=1:nrow(.))%>%
  glimpse()


# Basic plots to check out the data----

# functions for summarising data in plots
se <- function(x) sd(x) / sqrt(length(x))
se.min <- function(x) (mean(x)) - se(x)
se.max <- function(x) (mean(x)) + se(x)


# Barplot By Status----
ggplot(dat.summary, aes(x=status, y=count,fill=status)) + 
  stat_summary(fun.y=mean, geom="bar") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1)+
facet_grid(metric~sanctuary, scales = "free")




# Write the summary data----
setwd(data.dir) #set the directory
dir() #look in the directory
write_csv(dat.summary,paste(study,"summary",Sys.Date(),"csv",sep = "."))


