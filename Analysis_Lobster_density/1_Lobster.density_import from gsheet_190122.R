# BIOL4408 Marine Ecology field trip

###### Import lobster density data from gsheet ######
### Written by Tim Langlois 


##  What are we going to do with the data?----
##  1. Import data for gsheet



# Clear the workspace--
rm(list=ls())



# load librarys----
library(googlesheets4) #to read gsheet
library(tidyr) #to tody data
library(dplyr) #to transform data
library(forcats) #to transform catagorical data
library(readr) #to write data
library(ggplot2) #to plot data
library(here)


# Set name for study--
study<-"lobster.density"


# Set work directory----
# This is one way of setting a working directory
# Set your own to match where the data sits on your computer

work.dir=("~/GitHub/BIOL4408/Analysis_Lobster_density") #for Tim's laptop

work.dir=("~/workspace/BIOL4408/Analysis_Lobster_density") #for ecocloud server
# or
#work.dir=("") #set this for your computer work directory

# Set sub-directories----
data.dir=paste(work.dir,"Data",sep="/")


# But this can break!

# So we are going to use here()

setwd(here("Analysis_Lobster_density","Data"))
dir()





# Read in the data from gsheet and check it----

# # For Rstudio Desktop
options(httr_oob_default=FALSE)
# 
# # For Rstudio Server
# options(httr_oob_default=TRUE) 
# gs_auth(new_user = TRUE)


url <- "https://docs.google.com/spreadsheets/d/1Wqn7m2jopx11n5fdl9MHZAujRVkjBusdmIHq_gMhf0A/edit#gid=25814706"
dat<-read_sheet(url, sheet = "lobster.density")%>%
  as_tibble()%>%
  glimpse()
  
 dat<-dat%>%
  select(-c('longitude','latitude','time','way.point','gps','depth','group'))%>%
  glimpse()
 


# Write the gsheet data----
 setwd(here("Analysis_Lobster_density","Data"))
 dir() #look in the directory

# Write dat using study name and system date
write_csv(dat,paste(study,"gsheet","csv",sep = "."))



