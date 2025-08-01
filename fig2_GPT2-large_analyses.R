rm(list=ls());gc()
library(dplyr)
library(ggplot2)
library(tidyverse)
library(svglite)

#Functions#
min_max_norm<-function(x){(x - min(x,na.rm=T))/(max(x,na.rm=T) - min(x,na.rm=T))}

datapath<-"" #set path to where data is saved
savepath<-"" #set path to where you want to save the figures 

dt<-read.csv(paste(datapath, "GPT2-large-dimensions.csv", sep=""))
dt$Age.Score<-dt$age.main
dt$Gender.Score<-dt$gender.main

cor.test(dt$Age.Score, dt$Gender.Score)
dt$gender_norm<-min_max_norm(dt$Gender.Score)
dt$age_norm<-min_max_norm(dt$Age.Score)
cor.test(dt$age_norm, dt$gender_norm)

dt<-dt %>% mutate(gender_norm_rank = rank(-gender_norm), age_norm_rank = rank(-age_norm))
dt$combo_rank<-dt$gender_norm_rank + dt$age_norm_rank
dt<-dt %>% arrange(combo_rank)
head(dt, 60)
red_categories<-c("military personnel", "chief of staff", "elected official", "chairman of the board", "director of research")

dt<-dt %>% arrange(-combo_rank)
head(dt, 100)
blue_categories<-c("intern", "homoeopath", "novice", "secretary", "cook") #congresswoman
subset(dt, Social.Category %in% blue_categories)

category_class_df<-c()

for(category in unique(dt$Social.Category)){
  print(category)
  if(category %in% red_categories){class<-"M"}
  else if(category %in% blue_categories){class<-"W"}
  else{class<-"N"}
  category_class_df<-rbind(category_class_df, data.frame(Social.Category=category, class=class))
}

dt_m<-merge(dt, category_class_df, by=c("Social.Category"))
cor.test(dt_m$gender_norm, dt_m$age_norm)

#----#
#Plot#
#----#

savepath<-""

#windows()

ggplot(dt_m, aes(x = gender_norm, y = age_norm, fill=class, size=class, alpha=class)) + theme_bw() +
  geom_point(shape = 21, color = "grey50", stroke=1) + 
  scale_fill_manual(values=c("dodgerblue", "NA", "goldenrod")) + 
  scale_size_manual(values=c(14, 7, 14)) + 
  scale_alpha_manual(values=c(1, 0.6, 1)) + 
  xlab("Gender Association\n(Female-Male Dimension)") + ylab("Age Association\n(Young-Old Dimension)") + theme(
    axis.title.x=element_text(size=50),
    axis.title.y=element_text(size=50),
    plot.title=element_text(size=50, hjust=0.5),
    legend.text=element_text(size=50),
    legend.title=element_blank(),
    legend.position="none",
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black",fill="white", linewidth=1.4),
    axis.text.x=element_text(size = 50, vjust=0.8),axis.text.y=element_text(size = 50),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, aes(fill = NULL, size = NULL, alpha = NULL), 
              linewidth = 1, color="red", data = dt)

ggsave('fig2.svg', width=18, height=18, path = savepath)














