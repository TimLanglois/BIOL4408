# BIOL4408 Marine Ecology field trip


###### Format lobster density data for PRIMER ######
### Written by Tim Langlois 



##  What are we going to do ?----

# 1. Format data to PRIMER format - with samples as Rows and Species as coloums
    # for both 
    # a. Lobster data
    # b. Covariates
# 2. With Factors, to the right, seperated by blank columns .


# Clear the workspace--
rm(list=ls())


# librarys----
library(tidyr)
library(plyr) #older R library that has a function we need - can cause conflicts with dplyr()
library(dplyr)
library(readr)
library(RCurl) #needed to download data from GitHub


# Set name for study--
study<-"lobster.density"


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

work.dir=("~/GitHub/BIOL4408/Analysis_Lobster_density") #for Tim's desktop

work.dir=("~/workspace/BIOL4408/Analysis_Lobster_density") #for ecocloud server
# or
#work.dir=("") #set this for your computer work directory





# Set sub-directories----
data.dir=paste(work.dir,"Data",sep="/")
primer.dir=paste(work.dir,"Primer",sep="/")





# Read in the checked data-----
setwd(data.dir)
dir()
dat<-read.csv("lobster.density.csv")%>%
  glimpse()

# OR
#Read from github
dat<-read.csv(text=getURL("https://raw.githubusercontent.com/TimLanglois/BIOL4408/master/Analysis_Lobster_density/Data/lobster.density.csv"))



# Makes response variable data----
glimpse(dat)

response<-dat%>%
  select(sample.no,count,size.class)%>%
  spread(size.class,count, fill = 0)%>% #to make the data wide for PRIMER
  glimpse()


# Make the factor data----
factors<-dat%>%
  select(c(sample.no,sanctuary,status,site.new,year))%>%
  distinct()%>% #only unique combinations - to match the wide data
  glimpse()



# Make the covariate data----
covariates<-dat%>%
  select(sample.no,complexity,algal.cover,year)%>%
  distinct()%>%
  glimpse()



response.factors<-factors%>%
  inner_join(response,by="sample.no")%>% #join the data
  select(sample.no,legal,sub.legal,everything())%>% #orders the colums
  append_col(., list(blank=NA), after="sub.legal")%>% #appends blank colum
  plyr::rename(.,replace =c("blank"="") )%>% #makes the column name blank
  glimpse()


covariate.factors<-covariates%>%
  inner_join(factors,by="sample.no")%>%
  append_col(., list(blank=NA), after="year.x")%>%
  plyr::rename(.,replace =c("blank"="") )%>%
  glimpse()
  


# Write the data----
setwd(primer.dir)
dir()
write.csv(response.factors,file=paste("response.factors",study,"csv",sep = "."), row.names=FALSE)

write.csv(covariate.factors,file=paste("covariate.factors",study,"csv",sep = "."), row.names=FALSE)


