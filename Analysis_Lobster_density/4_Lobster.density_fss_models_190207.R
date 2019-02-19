# BIOL4408 Marine Ecology field trip

###### Import lobster density data and model ######
### Written by Tim Langlois 


##  What are we going to do ?----
##  1. Import checked data
##  2. model using FSSgam

# A simple function for full subsets multiple regression in ecology with R
# 
# R. Fisher
# S.K. Wilson
# S.M. Sin
# A.C. Lee
# Dr Tim J. Langlois


# Clear the workspace--
rm(list=ls())



# librarys----
detach("package:plyr", unload=TRUE)#will error - don't worry
library(tidyr)
library(dplyr)
library(mgcv)
library(gamm4)
library(MuMIn)
library(car)
library(doBy)
library(gplots)
library(RColorBrewer)
library(doSNOW)
library(RCurl) #needed to download data from GitHub
# install FSSgam package----
# devtools::install_github("beckyfisher/FSSgam_package") #run once
library(FSSgam)

rm(list=ls())

# Set name for study--
study<-"lobster.density"
# Need to set this for model outputs
name<-"lobster.density"



# Set work directory----
# Set your own to match where the data sits on your computer

work.dir=("~/GitHub/BIOL4408/Analysis_Lobster_density") #for Tim's desktop

work.dir=("~/workspace/BIOL4408/Analysis_Lobster_density") #for ecocloud server
# or
#work.dir=("") #set this for your computer work directory



# Set sub-directories----
data.dir=paste(work.dir,"Data",sep="/")
model.dir=paste(work.dir,"ModelOut",sep="/")



# Read in the checked data-----
setwd(data.dir)
dir()
# From local files
dat.raw<-read.csv("lobster.density.csv")%>%
  glimpse()

# OR
#Read from github
dat.raw<-read.csv(text=getURL("https://raw.githubusercontent.com/TimLanglois/BIOL4408/master/Analysis_Lobster_density/Data/lobster.density.csv"))



# Check out the data----
dat<-dat.raw%>%
  dplyr::rename(response=count,group=size.class)%>% #rename variables
glimpse()
# count is response
# size.class is group 


# Set continous predictor variables----
cont.pred.vars=c("complexity","algal.cover","year") 


# Check for correalation of contionus predictor variables----
# remove anything highly correlated (>0.95)---
round(cor(dat[,cont.pred.vars]),2)

# complexity algal.cover year
# complexity        1.00        0.06 0.16
# algal.cover       0.06        1.00 0.11
# year              0.16        0.11 1.00
# nothing is highly correlated 


# Review of individual contionous predictors ----
# we have to make sure they have an even distribution---
# Plot of likely transformations - thanks to Anna Cresswell for this loop!
par(mfrow=c(3,2))
for (i in cont.pred.vars) {
  x<-dat[ ,i]
  x = as.numeric(unlist(x))
  hist((x))
  plot((x),main = paste(i))
  hist(sqrt(x))
  plot(sqrt(x))
  hist(log(x+1))
  plot(log(x+1))
}

#If the data are squewed to low numbers try sqrt>log or if squewed to high numbers try ^2 of ^3

# For this data:
# year - should be a linear predictor - as it not many levels 
# algae.cover - sa logx transorfation looks good  - should also be a linear predictor due to not many levels - BUT we know there is a problem with the variable over years - don't use it in these models
# complexity - great distribtuion as raw variable

  
# Read in data again with transformation and re-set continous predictor variables----

dat <-dat.raw%>% 
  #no need to do any transformations here
  dplyr::rename(response=count,group=size.class)%>% #rename variable
  group_by(group)%>%
  dplyr::mutate(random=rnorm(n(), mean=10, sd=2))%>% #Added a random predictor to make function work
  ungroup()%>%
  glimpse()


# Re-set continous predictor variables----
cont.pred.vars=c("complexity","random") 

# BECKY - how come I have to have two cont.pred.vars?


#If I only have the "complexity" pred var. The function throws the error 
# "Error in generate.model.set(use.dat = use.dat, test.fit = Model1, pred.vars.cont = cont.pred.vars,  : 
#   Model max.predictors is greater than the number of predictors."

# I would have thought I could run the model with only:
#   cont.pred.vars=c("complexity")
#   lin.pred.vars=c("year") 
#   factor.vars=c("status")

# Is the function not counting one of the lin.pred.vars or factor.vars in its count of Model max.predictors?
  
  
  

# Set linear predictor variables----
lin.pred.vars=c("year") 


# Set factor predictor variables----
factor.vars=c("status")# Status as a Factor with two levels, "sanctuary" as a facotr with 3 levels


# Check to make sure Response vector has not more than 80% zeros----
unique.vars=unique(as.character(dat$group))
unique.vars.use=character()
for(i in 1:length(unique.vars)){
  temp.dat=dat[which(dat$group==unique.vars[i]),]
  if(length(which(temp.dat$response==0))/nrow(temp.dat)<0.9){ #have change this to allow 90% zeros
    unique.vars.use=c(unique.vars.use,unique.vars[i])}
}

unique.vars.use  

# We have ~90% zeros in the legal lobster data - not many legal lobster. We will need to account for this zero inflation in the error distribution we choose to use



# Run the FSSgam function----
setwd(model.dir)

# Set variables
resp.vars=unique.vars.use
use.dat=dat

# Make blank list for the outputs
out.all=list()
var.imp=list()


# Inspect the model set for FSSgam

Model1=gam(response~ s(complexity,bs="cr",k=3)+s(sanctuary,site.new,bs="re"),
           family=tw(),  data=use.dat)

model.set=generate.model.set(use.dat=use.dat,
                             test.fit=Model1,
                             pred.vars.cont=cont.pred.vars,
                             linear.vars=lin.pred.vars,
                             pred.vars.fact=factor.vars,
                             k=3,
                             max.predictors = 3,
                             null.terms="s(site.new,bs='re')")

# Inspect the model formulas - just to check all the models that will be run...
glimpse(model.set$mod.formula)




# Loop through the FSS function for each group----
out.all=list()
var.imp=list()

for(i in 1:length(resp.vars)){
  use.dat=dat[which(dat$group==resp.vars[i]),]
  
  Model1=gam(response~s(complexity,bs="cr",k=3)+s(sanctuary,site.new,bs="re"),
             family=tw(),  data=use.dat)
  
  model.set=generate.model.set(use.dat=use.dat,
                               test.fit=Model1,
                               pred.vars.cont=cont.pred.vars,
                               linear.vars=lin.pred.vars,
                               pred.vars.fact=factor.vars,
                               k=3,
                               max.predictors = 3,
                               null.terms="s(sanctuary,site.new,bs='re')")
  
  # Runs the actuall FSSgam function--
  out.list=fit.model.set(model.set,
                         max.models=600,
                         parallel=T)
  names(out.list)
  
  out.list$failed.models # examine the list of failed models
  mod.table=out.list$mod.data.out  # look at the model selection table
  mod.table=mod.table[order(mod.table$AICc),]
  mod.table$cumsum.wi=cumsum(mod.table$wi.AICc)
  out.i=mod.table[which(mod.table$delta.AICc<=3),]
  out.all=c(out.all,list(out.i))
  var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) # raw importance score
  
  # plot the best models
  for(m in 1:nrow(out.i)){
    best.model.name=as.character(out.i$modname[m])
    
    png(file=paste(name,m,resp.vars[i],"mod_fits.png",sep="_"))
    if(best.model.name!="null"){
      par(mfrow=c(3,1),mar=c(9,4,3,1))
      best.model=out.list$success.models[[best.model.name]]
      plot(best.model,all.terms=T,pages=1,residuals=T,pch=16)
      mtext(side=2,text=resp.vars[i],outer=F)}  
    dev.off()
  }
}




# Write the results of the FSSgam to folder----
setwd(model.dir)

names(out.all)=resp.vars
names(var.imp)=resp.vars
all.mod.fits=do.call("rbind",out.all)
all.var.imp=do.call("rbind",var.imp)
write.csv(all.mod.fits[,-2],file=paste(name,"all.mod.fits.csv",sep="_"))
write.csv(all.var.imp,file=paste(name,"all.var.imp.csv",sep="_"))



