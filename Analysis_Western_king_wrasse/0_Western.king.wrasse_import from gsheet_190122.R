# BIOL4408 Marine Ecology field trip

###### Import western king wrasse data from gsheet ######
### Written by Tim Langlois 


##  What are we going to do with the data?----
##  1. Import data




# Clear the workspace--
rm(list=ls())



# load librarys----
library(googlesheets) #to read gsheet
library(tidyr) #to tody data
library(dplyr) #to transform data
library(forcats) #to transform catagorical data
library(readr) #to write data





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





# Read in the data from gsheet and check it----

# # For Rstudio Desktop
# options(httr_oob_default=FALSE) 

# # For Rstudio Server
# options(httr_oob_default=TRUE) 
# gs_auth(new_user = TRUE) 


gs_ls() #list gsheets you have access to

gsheet.dat <- gs_title("BIOL4408.western.king.wrasse")%>% #select the gsheet
  gs_read_csv(ws = "western.king.wrasse") #select the worksheet within the workbook


glimpse(gsheet.dat) #glimpse the data
#variable names, formats, head of the data

  
  # Save the gsheet data----
setwd(data.dir) #set the directory
  dir() #look in the directory
  write_csv(gsheet.dat,paste(study,"gsheet.dat","csv",sep = "."))
  
