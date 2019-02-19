# BIOL4408 Marine Ecology field trip

###### Plots of lobster density importance scores ######
### Written by Tim Langlois 


##  What are we going to do ?----
##  1. Plot importance

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
    axis.title.x=element_text(vjust=0.3, size=12),
    axis.title.y=element_text(vjust=0.6, angle=90, size=12),
    axis.text.x=element_text(size=10,angle = 0),
    
    # axis.text.x=element_text(size=10,angle = 0, hjust=1,vjust=0.5),
    axis.text.y=element_text(size=10,face="bold"),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())







# PLOT  ----
setwd(model.dir)
dir()
dat.taxa<-read.csv("lobster.density_all.var.imp.csv")%>%
  rename(resp.var=X)%>%
  select(resp.var,status,sanctuary,complexity,year)%>%
  filter(resp.var%in%c("legal","sub.legal"))%>%
  # mutate(status=-status)%>%
  gather(key=predictor,value=importance,2:ncol(.))%>%
  # # mutate(importance=ifelse(predictor=="wTemp"&resp.var=="Lethrinus miniatus",-importance,importance))%>%
  # mutate(resp.var = fct_relevel(resp.var,"Choerodon rubescens","Lethrinus miniatus","Chrysophorus auratus","Epinephelides armatus","Coris auricularis","Neatypus obliquus"))%>%
  droplevels()%>%
  glimpse()



glimpse(dat.taxa)
unique(dat.taxa$resp.var)
unique(dat.taxa$predictor)



# Annotations-
dat.taxa.label<-dat.taxa%>%
  mutate(label=NA)%>%
  mutate(label=ifelse(predictor=="status"&resp.var=="legal","X",ifelse(predictor=="sanctuary"&resp.var=="legal","X",ifelse(predictor=="complexity"&resp.var=="legal","X",ifelse(predictor=="status"&resp.var=="sub.legal","X",ifelse(predictor=="year"&resp.var=="sub.legal","X",ifelse(predictor=="complexity"&resp.var=="sub.legal","X",label)))))))%>%
  # mutate(importance=-importance)%>%
  
  glimpse()




# Plotting----
# set colour ramps
bl <- colorRampPalette(c("navy","royalblue","lightskyblue"))(200)                      
re <- colorRampPalette(c("mistyrose", "red2","darkred"))(200)

bl <- colorRampPalette(c("royalblue","lightskyblue"))(200)                      
re <- colorRampPalette(c("mistyrose", "red2"))(200)

summary(dat.taxa.label$importance)



# PLOT total and species rich----
ggheat.potbot <- ggplot(dat.taxa.label, aes(x=predictor,y=resp.var,fill=importance))+ 
  # geom_tile(show.legend=F) + 
  geom_tile(alpha=0.75,show.legend=F) + 
  
  scale_fill_gradient(low = "white",high = "red")+
  # scale_fill_gradientn(colours=c(bl,"white", re), na.value = "grey98",
                       # limits = c(-1, 0.653))+
  # scale_fill_gradientn(colours=c("white", bl), na.value = "grey98",
                       # limits = c(0.2, 1))+
  # scale_fill_distiller(palette = "RdBu")+
  
  scale_x_discrete(limits=c("status","complexity",
                            "year",
                            "sanctuary"
                            
                            ),
                   labels=c("Status","Habitat \n complexity",
                            "Year",
                            "Sanctuary \n location"))+
  scale_y_discrete(limits = rev(levels(dat.taxa$resp.var)),labels=c("          Sub-legal","Legal"))+
  xlab(NULL)+
  ylab(NULL)+
  theme_classic()+
  # scale_y_discrete(limits = rev(levels(dat.taxa$resp.var)))+
  
  geom_text(aes(label=label))+
Theme1
# theme(axis.text.x=element_blank())
ggheat.potbot


# Save the plot----
setwd(plots.dir)

ggsave(ggheat.potbot,file="lobster.importance.png", width = 10, height = 5,units = "cm")
ggsave(ggheat.potbot,file="lobster.importance.parsim.png", width = 10, height = 5,units = "cm")



