# BIOL4408 Marine Ecology field trip

###### Import lobster density data and plot ######
### Written by Tim Langlois 


##  What are we going to do ?----
##  1. Import checked data
##  2. Do example plots we may need to accompany data analysis results


# Clear the workspace--
rm(list=ls())



# librarys----
library(tidyr)
library(dplyr) 
library(readr)
library(ggplot2)
library(RCurl) #needed to download data from GitHub


# Set name for study--
study<-"lobster.density"



# Functions----
# functions for summarising data in plots
se <- function(x) sd(x) / sqrt(length(x))
se.min <- function(x) (mean(x)) - se(x)
se.max <- function(x) (mean(x)) + se(x)







# Set work directory----
# Set your own to match where the data sits on your computer

work.dir=("~/GitHub/BIOL4408/Analysis_Lobster_density") #for Tim's desktop

work.dir=("~/workspace/BIOL4408/Analysis_Lobster_density") #for ecocloud server
# or
#work.dir=("") #set this for your computer work directory



# Set sub-directories----
data.dir=paste(work.dir,"Data",sep="/")
plot.dir=paste(work.dir,"Plots",sep="/")





# Read in the checked data-----
setwd(data.dir)
dir()
# From local files
dat<-read.csv("lobster.density.csv")%>%
  glimpse()

# OR
#Read from github
dat<-read.csv(text=getURL("https://raw.githubusercontent.com/TimLanglois/BIOL4408/master/Analysis_Lobster_density/Data/lobster.density.csv"))




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
  facet_grid(size.class~.) #facet by factor


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






# Bar plots - using functions ----
setwd(plot.dir)
dir()


# Barplot By Status----
ggplot(dat%>%filter(size.class=="legal"), aes(x=status, y=count,fill=status)) + 
  stat_summary(fun.y=mean, geom="bar") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  ggtitle("Legal lobster") #add a title
# facet_grid(.~sanctuary)


# Save the plot----
ggsave("status.barplot.png") #save the last plot made - can be confuding when you have lots of plots



# Barplot By Status----
status.barplot<-
ggplot(dat%>%filter(size.class=="legal"), aes(x=status, y=count,fill=status)) + 
  stat_summary(fun.y=mean, geom="bar") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  ggtitle("Legal lobster")+
  facet_grid(.~sanctuary)
status.barplot

ggsave("status.barplot.png",status.barplot,width = 15, height = 8,units = "cm")




# Barplot By Status and size class----
ggbarplot.status.size<-ggplot(dat, aes(x=status, y=count,fill=status)) + 
  stat_summary(fun.y=mean, geom="bar") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1)+
  facet_grid(size.class~)
# What's wrong with this facet?
ggbarplot.status.size







# Very smart plot using Themes----


# Themes for plotting----
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=15),
    legend.title = element_blank(),
    legend.position = "top",
    text=element_text(size=14),
    strip.text.y = element_text(size = 14,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=15),
    axis.title.y=element_text(vjust=0.6, angle=90, size=15),
    # axis.text.x=element_blank(),
    axis.text.y=element_text(size=14),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())



status.year.sanctuary<-ggplot(dat%>%filter(size.class=="legal"), aes(x=status, y=count,fill=status)) + 
  stat_summary(fun.y=mean, geom="bar", colour="black") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  # Labels
  xlab("Marine sanctuary")+
  ylab(bquote('Density of legal rock lobster (no. /10 m'^2*')'))+
  ggtitle("Density of legal lobster")+
  # Apperance
  theme_bw()+
  Theme1+
  facet_grid(year~sanctuary)
status.year.sanctuary

ggsave(status.year.sanctuary,file="status.year.sanctuary.png",width = 15, height = 15,units = "cm")




# You can make MANY changes to the appreance of these plots----
# Suggested reading:
#   Google
#   http://www.cookbook-r.com/Graphs/





# Temporal plot----
glimpse(dat)

status.year.size<-ggplot(dat, aes(x=year, y=count,colour=status)) + 
  stat_summary(fun.y=mean, geom="point") +
  stat_summary(fun.y=mean, geom="line") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  theme_bw()+
  Theme1+
  #Add a facet?
  facet_grid(size.class~sanctuary,scale="free")
  
status.year.size





# Plot of lobster with co-variate geom_smooth()----
# geom_smooth()? #be careful with geom_smooth()


legal.status.complexity<-
  ggplot(dat%>%filter(size.class=="legal"), aes(x=complexity, y=count,colour=status)) + 
  # smoother - by defauly uses loess and adds a standard error
  geom_smooth()+
  # geom_smooth(method=lm, size=0.5,se=F)+
  theme_bw()+
  facet_grid(sanctuary~.)
legal.status.complexity




