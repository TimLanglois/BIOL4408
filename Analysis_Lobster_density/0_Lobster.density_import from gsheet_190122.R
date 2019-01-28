# BIOL4408 Marine Ecology field trip

###### Import lobster density data from gsheet ######
### Written by Tim Langlois 


##  What are we going to do with the data?----
##  1. Import data for gsheet



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

work.dir=("~/GitHub/BIOL4408/Analysis_Lobster_density") #for Tim's desktop

work.dir=("~/workspace/BIOL4408/Analysis_Lobster_density") #for ecocloud server
# or
#work.dir=("") #set this for your computer work directory



# Set sub-directories----
data.dir=paste(work.dir,"Data",sep="/")





# Read in the data from gsheet and check it----

# # For Rstudio Desktop
# options(httr_oob_default=FALSE) 
# 
# # For Rstudio Server
# options(httr_oob_default=TRUE) 
# gs_auth(new_user = TRUE) 


gs_ls() #list gsheets you have access to

dat <- gs_title("BIOL4408.lobster.density")%>% #select the gsheet
  gs_read_csv(ws = "lobster.density")%>% #select the worksheet within the workbook
glimpse()

# Write the gsheet data----
setwd(data.dir) #set the directory
dir() #look in the directory

# Write dat using study name and system date
write_csv(dat,paste(study,"gsheet","csv",sep = "."))



