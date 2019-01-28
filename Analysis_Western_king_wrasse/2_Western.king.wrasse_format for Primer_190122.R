# BIOL4408 Marine Ecology field trip


###### Format western king wrasse data for PRIMER ######
### Written by Tim Langlois 


##  What are we going to do ?----

# 1. Make PRIMER data for Length and abundance of types
# 2. Format data to PRIMER format - with samples as Rows and variables as coloums
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
study<-"western.king.wrasse"


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

work.dir=("~/workspace/BIOL4408/Analysis_Western_king_wrasse") #for ecocloud server
# or
#work.dir=("") #set this for your computer work directory





# Set sub-directories----
data.dir=paste(work.dir,"Data",sep="/")
primer.dir=paste(work.dir,"Primer",sep="/")





# Read in the checked data-----
setwd(data.dir)
dir()

# From local files

length.dat<-read.csv("western.king.wrasse.csv")%>%
  glimpse()

sum.dat<-read.csv("western.king.wrasse.summary.csv")%>%
  glimpse()

ratio.dat<-read.csv("western.king.wrasse.ratio.csv")%>%
  glimpse()


# OR
#Read from github
length.dat<-read.csv(text=getURL("https://raw.githubusercontent.com/TimLanglois/BIOL4408/master/Analysis_Western_king_wrasse/Data/western.king.wrasse.csv"))

sum.dat<-read.csv(text=getURL("https://raw.githubusercontent.com/TimLanglois/BIOL4408/master/Analysis_Western_king_wrasse/Data/western.king.wrasse.summary.csv"))

ratio.dat<-read.csv(text=getURL("https://raw.githubusercontent.com/TimLanglois/BIOL4408/master/Analysis_Western_king_wrasse/Data/western.king.wrasse.ratio.csv"))





# Primer data for summary data-----
# Makes response variable data----
glimpse(sum.dat)

sum.response<-sum.dat%>%
  select(sample.no,count,metric)%>%
  spread(metric,count, fill = 0)%>% #to make the dat.sum wide for PRIMER
  glimpse()


# Make the factor dat.suma----
sum.factors<-sum.dat%>%
  select(c(sample.no,sanctuary,status,site))%>%
  distinct()%>% #only unique combinations - to match the wide dat.suma
  glimpse()


sum.response.factors<-sum.factors%>%
  inner_join(sum.response,by="sample.no")%>% #join the data
  select(sample.no,M,`F`,J,school.count,everything())%>% #orders the colums
  append_col(., list(blank=NA), after="school.count")%>% #appends blank colum
  plyr::rename(.,replace =c("blank"="") )%>% #makes the column name blank
  glimpse()


# Write the data----
setwd(primer.dir)
dir()
write.csv(sum.response.factors,file=paste("sum.response.factors",study,"csv",sep = "."), row.names=FALSE)



# Primer data for ratio data-----
# Makes response variable data----
glimpse(ratio.dat)

ratio.response<-ratio.dat%>%
  select(transect.id,count)%>%
  glimpse()


# Make the factor dat.suma----
ratio.factors<-ratio.dat%>%
  select(c(transect.id,sanctuary,status,site))%>%
  distinct()%>% #only unique combinations - to match the wide dat.suma
  glimpse()


ratio.response.factors<-ratio.factors%>%
  inner_join(ratio.response,by="transect.id")%>% #join the data
  select(transect.id,count,everything())%>% #orders the colums
  append_col(., list(blank=NA), after="count")%>% #appends blank colum
  plyr::rename(.,replace =c("blank"="") )%>% #makes the column name blank
  glimpse()


# Write the data----
setwd(primer.dir)
dir()
write.csv(ratio.response.factors,file=paste("ratio.response.factors",study,"csv",sep = "."), row.names=FALSE)




# Primer data for length data-----
# Makes response variable data----
glimpse(length.dat)

length.response<-length.dat%>%
  select(row.id,length.mm)%>%
  filter(!is.na(length.mm))%>%
  glimpse()


# Make the factor dat.suma----
length.factors<-length.dat%>%
  select(c(row.id,sanctuary,status,site,stage,transect.id))%>% # needs repliacte /transect name in there ALSO need stage in there.
  distinct()%>% #only unique combinations - to match the wide dat.suma
  glimpse()

length.response.factors<-length.factors%>%
  inner_join(length.response,by="row.id")%>% #join the data
  select(row.id,length.mm,everything())%>% #orders the colums
  append_col(., list(blank=NA), after="length.mm")%>% #appends blank colum
  plyr::rename(.,replace =c("blank"="") )%>% #makes the column name blank
  glimpse()

# Write the data----
setwd(primer.dir)
dir()
write.csv(length.response.factors,file=paste("length.response.factors",study,"csv",sep = "."), row.names=FALSE)

