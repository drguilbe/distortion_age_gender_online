rm(list=ls());gc()
library(dplyr)
library(ggplot2)
library(tidyverse)

#Functions#
min_max_norm <- function(x, min_x, max_x){
  norm_x = (x - max_x)/(min_x - max_x)
  return(norm_x)
}

dt<-read.csv("G:/My Drive/Research/Labs/COMPSYN/gendered ageism/data/Nature_Submit/GPT2-large-dimensions.csv")

cor.test(dt$Age.Score, dt$Gender.Score)

min_gender<-min(dt$Gender.Score * -1)
max_gender<-max(dt$Gender.Score * -1)
dt$gender_norm<-sapply(1:nrow(dt), function(x) min_max_norm(dt[x,]$Gender.Score * -1, min_gender, max_gender))

min_age<-min(dt$Age.Score * -1)
max_age<-max(dt$Age.Score * -1)
dt$age_norm<-sapply(1:nrow(dt), function(x) min_max_norm(dt[x,]$Age.Score * -1, min_age, max_age))

cor.test(dt$age_norm, dt$gender_norm)

dt<-dt %>% mutate(gender_norm_rank = rank(-gender_norm), age_norm_rank = rank(-age_norm))
dt$combo_rank<-dt$gender_norm_rank + dt$age_norm_rank
dt<-dt %>% arrange(combo_rank)
head(dt, 60)
red_categories<-c("military personnel", "chief of staff", "elected official", "chairman of the board", "director of research")

dt<-dt %>% arrange(-combo_rank)
head(dt, 100)
blue_categories<-c("intern", "homoeopath", "novice", "secretary", "cook") #congresswoman
subset(dt, Category %in% blue_categories)

category_class_df<-c()

for(category in unique(dt$Category)){
  print(category)
  if(category %in% red_categories){class<-"M"}
  else if(category %in% blue_categories){class<-"W"}
  else{class<-"N"}
  category_class_df<-rbind(category_class_df, data.frame(Category=category, class=class))
}

dt_m<-merge(dt, category_class_df, by=c("Category"))
cor.test(dt_m$gender_norm, dt_m$age_norm)

#----#
#Plot#
#----#

savepath<-""

#windows()

ggplot(dt_m, aes(x = gender_norm, y = age_norm, fill=class, size=class, alpha=class)) + theme_bw() +
  geom_point(shape = 21, color = "grey50", stroke=1) + 
  scale_fill_manual(values=c("dodgerblue", "NA", "goldenrod")) + 
  scale_size_manual(values=c(7, 3, 7)) + 
  scale_alpha_manual(values=c(1, 0.6, 1)) + 
  xlab("Gender Association\n(Female-Male Dimension)") + ylab("Age Association\n(Young-Old Dimension)") + theme(
    axis.title.x=element_text(size=35),
    axis.title.y=element_text(size=35),
    plot.title=element_text(size=35, hjust=0.5),
    legend.text=element_text(size=35),
    legend.title=element_blank(),
    legend.position="none",
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black",fill="white", linewidth=1.4),
    axis.text.x=element_text(size = 30, vjust=0.8),axis.text.y=element_text(size = 30),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, aes(fill = NULL, size = NULL, alpha = NULL), 
              linewidth = 1, color="red", data = dt)

#ggsave("fig3_gpt2-large.png", width=12, height=12, path = savepath)















