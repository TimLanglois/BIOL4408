# BIOL4408 Marine Ecology field trip


###### Format western king wrasse data for PRIMER ######
### Written by Tim Langlois 


##  What are we going to do ?----

# 1. Format data to PRIMER format - with samples as Rows and Species as coloums
# 2. With Factors, to the right, seperated by blank columns .


# Clear the workspace--
rm(list=ls())


# librarys----
library(tidyr)
library(plyr) #older R library that has a function we need - can cause conflicts with dplyr()
library(dplyr)
library(readr)


# Set name for study--
study<-"wester.king.wrasse"


# Functions----
# To append a column 
# Needed for PRIMER to append a blank column to distiguish Factors--
append_col <- function(x, cols, after=length(x)) {
  x <- as.data.frame(x)
  if (is.character(after)) {
    ind <- which(colnames(x) == after)
    if (any(is.null(ind))) stop(after, "not found in colnames(x)\n")
  } else if (is.numeric(after)) {
    ind <- after
  }
  stopifnot(all(ind <= ncol(x)))
  cbind(x, cols)[, append(1:ncol(x), ncol(x) + 1:length(cols), after=ind)]
}





# Set work directory----
# Set your own to match where the data sits on your computer

work.dir=("~/GitHub/BIOL4408/Analysis_Western_king_wrasse") #for Tim

#work.dir=("") #set this for your computer work directory





# Set sub-directories----
data.dir=paste(work.dir,"Data",sep="/")
primer.dir=paste(work.dir,"Primer",sep="/")





# Read in the checked data-----
setwd(data.dir)
dir()
dat<-read.csv("wester.king.wrasse.summary.2019-01-22.csv")%>%
  glimpse()




# Makes response variable data----
glimpse(dat)

response<-dat%>%
  select(sample.no,count,metric)%>%
  spread(metric,count, fill = 0)%>% #to make the data wide for PRIMER
  glimpse()


# Make the factor data----
factors<-dat%>%
  select(c(sample.no,sanctuary,status,site))%>%
  distinct()%>% #only unique combinations - to match the wide data
  glimpse()




response.factors<-factors%>%
  inner_join(response,by="sample.no")%>% #join the data
  select(sample.no,M,`F`,J,MtoF,school.count,everything())%>% #orders the colums
  append_col(., list(blank=NA), after="school.count")%>% #appends blank colum
  plyr::rename(.,replace =c("blank"="") )%>% #makes the column name blank
  glimpse()




# Write the data----
setwd(primer.dir)
dir()
write.csv(response.factors,file=paste(study,"response.factors",Sys.Date(),"csv",sep = "."), row.names=FALSE)



