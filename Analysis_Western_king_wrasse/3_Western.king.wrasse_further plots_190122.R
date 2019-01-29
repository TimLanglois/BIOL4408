# BIOL4408 Marine Ecology field trip

###### Import western king wrasse data and plot ######
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
study<-"western.king.wrasse"



# Functions----
# functions for summarising data in plots
se <- function(x) sd(x) / sqrt(length(x))
se.min <- function(x) (mean(x)) - se(x)
se.max <- function(x) (mean(x)) + se(x)







# Set work directory----
# Set your own to match where the data sits on your computer

work.dir=("~/GitHub/BIOL4408/Analysis_Western_king_wrasse") #for Tim

work.dir=("~/workspace/BIOL4408/Analysis_Western_king_wrasse") #for ecocloud server
# or
#work.dir=("") #set this for your computer work directory



# Set sub-directories----
data.dir=paste(work.dir,"Data",sep="/")
plot.dir=paste(work.dir,"Plots",sep="/")




# Read in the checked data-----
# From local files
setwd(data.dir)
dir()

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






# Basic plots to check out the length data----

# Box plot (looks better with length data)----
ggplot(length.dat, aes(x=status, y=length.mm)) + 
  geom_boxplot(outlier.shape = NA, notch=FALSE, width=0.8)+
  geom_point(position = position_jitter(width = 0.1, h = 0),alpha = 1/4, size=1)+
  stat_summary(fun.y=mean, geom="point", shape=2, size=4)

ggplot(length.dat, aes(x=status, y=length.mm)) + 
  geom_boxplot(outlier.shape = NA, notch=FALSE, width=0.8)+
  geom_point(position = position_jitter(width = 0.1, h = 0),alpha = 1/4, size=1)+
  stat_summary(fun.y=mean, geom="point", shape=2, size=4)+
  facet_grid(stage~sanctuary)




# Basic plots to check out the summary and ratio data----


# Barplot By Status----
ggplot(sum.dat, aes(x=status, y=count,fill=status)) + 
  stat_summary(fun.y=mean, geom="bar") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1)+
  facet_grid(metric~sanctuary, scales = "free")

ggplot(ratio.dat, aes(x=status, y=count,fill=status)) + 
  stat_summary(fun.y=mean, geom="bar") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1)+
  facet_grid(.~sanctuary, scales = "free")



# # Smarter Bar plots 
setwd(plot.dir)
dir()




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



# Very smart plot using Themes----

glimpse(sum.dat)

status.location.sanctuary<-ggplot(sum.dat%>%filter(metric%in%c("M","F")), aes(x=status, y=count,fill=status)) +
  stat_summary(fun.y=mean, geom="bar", colour="black") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  # Labels
  xlab("Marine sanctuary")+
  ylab(bquote('Density of western king wrasse (no. /?? m'^2*')'))+
  ggtitle("Density of western king wrasse")+
  # Apperance
  theme_bw()+
  Theme1+
  facet_grid(metric~sanctuary,scales="free")
status.location.sanctuary

ggsave(status.year.sanctuary,file="status.year.sanctuary.png",width = 15, height = 15,units = "cm")




# You can make MANY changes to the appreance of these plots----
# Suggested reading:
#   Google
#   http://www.cookbook-r.com/Graphs/



