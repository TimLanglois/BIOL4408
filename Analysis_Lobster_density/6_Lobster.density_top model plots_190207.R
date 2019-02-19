# BIOL4408 Marine Ecology field trip

###### Plots of lobster density top models ######
### Written by Tim Langlois 


##  What are we going to do ?----
##  1. Plot top models

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
library(ggplot2)

rm(list=ls())

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
model.dir=paste(work.dir,"ModelOut",sep="/")
plots.dir=paste(work.dir,"Plots",sep="/")



# Set Themes for plotting ----
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.background = element_rect(fill="white"),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=10),
    legend.title = element_blank(),
    legend.position = "top",
    legend.direction="horizontal",
    text=element_text(size=10),
    strip.text.y = element_text(size = 10,angle = 0),
    strip.text.x = element_text(size = 12,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=12),
    axis.title.y=element_text(vjust=0.6, angle=90, size=12),
    # axis.text.x=element_blank(),
    
    axis.text.x=element_text(size=10,angle = 0, hjust=1,vjust=0.5),
    axis.text.y=element_text(size=10),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())




# Read in the checked data-----
setwd(data.dir)
dir()
# From local files
dat.raw<-read.csv("lobster.density.csv")%>%
  glimpse()

tail(dat)


# Check out the data----
dat<-dat.raw%>%
  dplyr::rename(response=count,group=size.class)%>% #rename variables
  glimpse()
# count is response
# size.class is group 





# Manually make the most parsimonious GAM models for each group ----

# MODEL Legal ~Stats*Complexity + Sanctuary ----


gamm.legal=gam(response~ status*complexity+sanctuary+s(site.new,bs="re"),
           family=tw(),  data=dat%>%filter(group=="legal"))



# PREDICT Legal ~Status*Complexity  ----

mod<-gamm.legal

testdata <- expand.grid(complexity=seq(min(dat$complexity),max(dat$complexity),length.out = 20),
                        sanctuary=(mod$model$sanctuary),
                        site.new=(mod$model$site.new),
                        status = c("Fished","No-take"))%>%
  distinct()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.legal.complex.x.status = testdata%>%data.frame(fits)%>%
  group_by(complexity,status)%>%
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()%>%
  glimpse()


legal.complexity.x.status<- ggplot(aes(x=complexity,y=response,colour=status), data=dat%>%filter(group=="legal")) +
  ylab("Density of >legal size lobster")+
  xlab('Habitat complexity')+
  ggtitle("Status x Habitat complexity")+
    scale_color_manual(labels = c("Fished", "No-take"),values=c("#999999", "#009E73"))+
  # geom_jitter(width = 0.25,height = 0,alpha=0.75, size=2,show.legend=FALSE)+
  # geom_point(alpha=0.75, size=2)+
  geom_line(data=predicts.legal.complex.x.status,show.legend=T, alpha=1,size=1)+
  geom_line(data=predicts.legal.complex.x.status,aes(y=response - se.fit),linetype="dashed",show.legend=FALSE, alpha=1,size=1)+
  geom_line(data=predicts.legal.complex.x.status,aes(y=response + se.fit),linetype="dashed",show.legend=FALSE, alpha=1,size=1)+
  theme_classic()+
  # annotate("text", x = -Inf, y=Inf, label = "Status x Habitat complexity",vjust = 1, hjust = -.1,size=5)+
  Theme1
legal.complexity.x.status

# Save the plot----
setwd(plots.dir)

ggsave(legal.complexity.x.status,file="legal.complexity.x.status.png", width = 15, height = 10,units = "cm")



# PREDICT Legal ~Stats + Sanctuary ----

unique(dat$sanctuary)
mod<-gamm.legal

testdata <- expand.grid(complexity=mean(mod$model$complexity),
                        sanctuary=c("Armstrong Bay","Parker Point","Green Island"),
                        site.new=(mod$model$site.new),
                        status = c("Fished","No-take"))%>%
  distinct()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.legal.sanct.status = testdata%>%data.frame(fits)%>%
  group_by(sanctuary,status)%>%
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()%>%
  glimpse()




legal.sanctuary.status<- ggplot(aes(x=status,y=response,fill=status,colour=status), data=predicts.legal.sanct.status) +
  ylab("Density of >legal size lobster")+
  xlab('Sanctuary location')+
    ggtitle("Sanctuary location")+
  scale_fill_manual(labels = c("Fished", "No-take"),values=c("#999999", "#009E73"))+
  scale_colour_manual(labels = c("Fished", "No-take"),values=c("#999999", "#009E73"))+
  # scale_x_discrete(limits = rev(levels(predicts.legal.sanct.status$status)))+
  geom_bar(stat = "identity",alpha=0.5)+
  geom_errorbar(aes(ymin = response-se.fit,ymax = response+se.fit),width = 0.5,alpha=1) +
  theme_classic()+
  Theme1+
  # annotate("text", x = -Inf, y=Inf, label = "Sanctuary location",vjust = 1, hjust = -.1,size=5)+
  # annotate("text", x = -Inf, y=Inf, label = "   Dosinia subrosea",vjust = 1, hjust = -.1,size=5,fontface="italic")
  facet_wrap(~sanctuary,strip.position="bottom")+
  theme(strip.placement="outside")
  # theme(title="A",plot.title=element.text(hjust=0))
legal.sanctuary.status

# Save the plot----
setwd(plots.dir)

ggsave(legal.sanctuary.status,file="legal.sanctuary.status.png", width = 15, height = 10,units = "cm")



# MODEL SubLegal ~Stats*Complexity + Sanctuary ----
unique(dat$group)

gamm.sub.legal=gam(response~ status*complexity+year+s(site.new,bs="re"),
               family=tw(),  data=dat%>%filter(group=="sub.legal"))



# PREDICT SubLegal ~Status*Complexity  ----

mod<-gamm.sub.legal

testdata <- expand.grid(year=seq(min(dat$year),max(dat$year),length.out = 20),
                        complexity=mean(mod$model$complexity),
                        site.new=(mod$model$site.new),
                        status = c("Fished","No-take"))%>%
  distinct()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sublegal.year = testdata%>%data.frame(fits)%>%
  group_by(year)%>%
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()%>%
  glimpse()


sublegal.year<- ggplot(aes(x=year,y=response), data=dat%>%filter(group=="sub.legal")) +
  ylab("Density of sub-legal size lobster")+
  xlab('Year')+
  ggtitle("Status x Habitat complexity")+
  scale_color_manual(labels = c("Fished", "No-take"),values=c("red", "black"))+
  # geom_jitter(width = 0.25,height = 0,alpha=0.75, size=2,show.legend=FALSE)+
  # geom_point(alpha=0.75, size=2)+
  geom_line(data=predicts.sublegal.year,show.legend=T, alpha=0.5)+
  geom_line(data=predicts.sublegal.year,aes(y=response - se.fit),linetype="dashed",show.legend=FALSE, alpha=0.5)+
  geom_line(data=predicts.sublegal.year,aes(y=response + se.fit),linetype="dashed",show.legend=FALSE, alpha=0.5)+
  theme_classic()+
  # annotate("text", x = -Inf, y=Inf, label = "Status x Habitat complexity",vjust = 1, hjust = -.1,size=5)+
  Theme1
sublegal.year

# Save the plot----
setwd(plots.dir)

ggsave(legal.complexity.x.status,file="legal.complexity.x.status.png", width = 15, height = 10,units = "cm")



