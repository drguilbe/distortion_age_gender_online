#Replication Script for Study of Gendered Ageism
#Douglas R. Guilbeault & Solene Delecourt
#Haas School of Business
#University of California, Berkeley

rm(list=ls());gc()
library(dplyr)
library(ggplot2)
library(tidyverse)
library(sjPlot) #https://strengejacke.github.io/sjPlot/articles/tab_mixed.html
library(clinfun)
library(ggridges);theme_set(theme_ridges())
library(jtools) #https://cran.r-project.org/web/packages/jtools/vignettes/summ.html
library(lmtest)
library(multiwayvcov)
library(rms)

#functions
min_max_norm<-function(x){(x - min(x,na.rm=T))/(max(x,na.rm=T) - min(x,na.rm=T))}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

savepath<-"C:/Users/dougl/Desktop/"

###########
#Load Data#
###########
data_raw<-read.csv("C:/Users/dougl/Desktop/comp-syn/algorithmic_bias/Data/RnR/all_data_final.csv")
data_clean<-subset(data_raw, Human.Face == "Yes" & Attention.Check)
data_binary<-subset(data_clean, Img.Gender != "Non-binary")
data_main<-subset(data_binary, !searchDEMO %in% c("Male", "Female"))

data_clean_agg<- data_clean %>% 
  group_by(Data.Source, Social.Category, searchDEMO, face_id, image_id) %>% 
  dplyr::summarise(Img.Gender.Mode=getmode(Img.Gender), 
                   Img.Age.Mode=getmode(Img.Age), 
                   Img.Age.Avg=mean(Img.Age,na.rm=T))

data_binary_agg<- data_binary %>% 
  group_by(Data.Source, Social.Category, searchDEMO, face_id, image_id) %>% 
  dplyr::summarise(Img.Gender.Mode=getmode(Img.Gender), 
                   Img.Age.Mode=getmode(Img.Age), 
                   Img.Age.Avg=mean(Img.Age,na.rm=T))

data_binary_agg<-data_binary_agg[!is.na(data_binary_agg$Social.Category),]

data_main_agg<- data_main %>% 
  group_by(Data.Source, Social.Category, face_id, image_id) %>% 
  dplyr::summarise(Img.Gender.Mode=getmode(Img.Gender), 
                   Img.Age.Mode=getmode(Img.Age), 
                   Img.Age.Avg=mean(Img.Age,na.rm=T))

data_main_agg_stats<-data_main_agg %>% 
  group_by(Data.Source, Img.Gender.Mode) %>% 
  dplyr::summarise(Avg.Img.Age.Num = mean(Img.Age.Mode),
                   Med.Img.Age.Num = median(Img.Age.Mode),
                   Mode.Img.Age.Num = getmode(Img.Age.Mode))

###############################
#IMDb Data (IMDb-Wiki Dataset)#
###############################
IMDBceleb<-read.csv("C:/Users/dougl/Desktop/comp-syn/algorithmic_bias/Data/IMDB_WIKI/imdb_age_celeb.csv")
IMDBceleb<-subset(IMDBceleb, gender %in% c(0,1))
IMDBceleb<-subset(IMDBceleb, age > 0 & age <= 100)
IMDBceleb$gender_cat<-as.factor(IMDBceleb$gender)
levels(IMDBceleb$gender_cat)<-c("Female", "Male")

####################################
#Wikipedia Data (IMDb-Wiki Dataset)#
####################################
wikiceleb<-read.csv("C:/Users/dougl/Desktop/comp-syn/algorithmic_bias/Data/IMDB_WIKI/wiki_age_celeb.csv")
wikiceleb<-subset(wikiceleb, gender %in% c(0,1))
wikiceleb<-subset(wikiceleb, age > 0 & age <= 100)
wikiceleb$gender_cat<-as.factor(wikiceleb$gender)
levels(wikiceleb$gender_cat)<-c("Female", "Male")

############################
#Google Data (CACD Dataset)#
############################
CADC<-read.csv("C:/Users/dougl/Desktop/comp-syn/algorithmic_bias/Data/RnR/CACD.csv")
IMDBceleb_binary<-unique(IMDBceleb %>% select(name, gender))
CADC_comp<-merge(IMDBceleb_binary, CADC, by=c("name"))
CADC_comp$gender<-as.factor(CADC_comp$gender)
levels(CADC_comp$gender)<-c("Female", "Male")

##############
#Main Results#
##############

################
####Figure 1####
################

#Fig. 1A#
google_main_agg<-subset(data_main_agg, Data.Source=="Google")
google_main_agg_stats<-subset(data_main_agg_stats, Data.Source=="Google")

ggplot(google_main_agg, aes(x = Img.Age.Avg, fill=Img.Gender.Mode, 
                        color=Img.Gender.Mode, group=Img.Gender.Mode, 
                        linetype=Img.Gender.Mode)) + 
  theme_bw() + geom_density(lwd = 2, colour = "black", alpha = 0.7, bw = 0.38) +
  scale_linetype_manual(values=c("dotted", "solid")) + 
  scale_fill_manual(values=c("goldenrod", "lightblue")) + 
  xlab("Age of Face") + ggtitle("Google") + 
  theme(legend.text=element_text(size=60),
        legend.position=c(0.85,0.88),
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.5, angle=37, vjust=0.7),
        axis.text.y=element_text(size = 60, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits=c(-1,8), 
                     breaks=c(1,2,3,4,5,6,7),
                     labels=c("0-11", "12-17","18-24","25-34","35-54","55-74","+75")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  coord_cartesian(ylim=c(0,0.45)) + 
  geom_vline(xintercept = subset(google_main_agg_stats, Img.Gender.Mode=="Female")$Avg.Img.Age.Num, 
             color="goldenrod", size=4, alpha=0.8, linetype="dashed") + 
  geom_vline(xintercept = subset(google_main_agg_stats, Img.Gender.Mode=="Male")$Avg.Img.Age.Num, 
             color="blue", size=4, alpha=0.8) 

#Fig 1A Stats#
t.test(subset(google_main_agg, Img.Gender.Mode=="Female")$Img.Age.Avg, 
       subset(google_main_agg, Img.Gender.Mode=="Male")$Img.Age.Avg)

#Fig 1A Stats (Matched by Category)#
google_main_cat_match<- google_main_agg %>% group_by(Social.Category) %>%
  dplyr::summarise(fem.age.avg=mean(Img.Age.Avg[Img.Gender.Mode=="Female"],na.rm=T),
                   fem.freq = length(Img.Age.Avg[Img.Gender.Mode=="Female"]), 
                   mal.age.avg=mean(Img.Age.Avg[Img.Gender.Mode=="Male"],na.rm=T), 
                   mal.freq = length(Img.Age.Avg[Img.Gender.Mode=="Male"]))

google_main_cat_match<-google_main_cat_match[complete.cases(google_main_cat_match),]
t.test(google_main_cat_match$fem.age.avg, google_main_cat_match$mal.age.avg,paired=T)

#Fig. 1B#
google_clean_agg<-subset(data_clean_agg, Data.Source=="Google")
google_gendered<-subset(google_clean_agg, searchDEMO != "None" & Img.Gender.Mode %in% c("Male","Female"))

google_gendered_agg<-google_gendered %>% group_by(Social.Category, Img.Gender.Mode) %>% 
  dplyr::summarise(Img.Age.Avg = mean(Img.Age.Avg, na.rm=T), 
                   num_faces = length(unique(face_id)))

ggplot(google_gendered_agg, aes(x = Img.Age.Avg, fill=Img.Gender.Mode, color=Img.Gender.Mode, group=Img.Gender.Mode, linetype=Img.Gender.Mode)) +
  theme_bw() + 
  geom_density(lwd = 2, colour = "black", alpha = 0.7, bw = 0.7) +
  scale_fill_manual(values=c("goldenrod", "lightblue")) + 
  scale_linetype_manual(values=c("dotted", "solid")) + 
  xlab("Age of Face") + ggtitle("Gendered Google Searches") + 
  theme(legend.text=element_text(size=60),
        legend.position=c(0.85,0.88),
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.5, angle=37, vjust=0.7),
        axis.text.y=element_text(size = 60, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits=c(1,7), 
                     breaks=c(1,2,3,4,5,6,7),
                     labels=c("0-11", "12-17","18-24","25-34","35-54","55-74","+75")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  geom_vline(xintercept = mean(subset(google_gendered_agg, Img.Gender.Mode=="Female")$Img.Age.Avg,na.rm=T),
             color="goldenrod", size=4, alpha=0.8, linetype="dashed") + 
  geom_vline(xintercept = mean(subset(google_gendered_agg, Img.Gender.Mode=="Male")$Img.Age.Avg,na.rm=T),
             color="blue", size=4, alpha=0.8) 

#Fig. 1B Stats#
t.test(subset(google_gendered_agg, Img.Gender.Mode=="Female")$Img.Age.Avg, 
       subset(google_gendered_agg, Img.Gender.Mode=="Male")$Img.Age.Avg)

t.test(google_gendered_agg_cat_match$fem.age.avg, 
       google_gendered_agg_cat_match$mal.age.avg,paired=T)

google_gendered_agg_cat_match<-google_gendered_agg %>% group_by(Social.Category) %>%
  dplyr::summarise(fem.age.avg=mean(Img.Age.Avg[Img.Gender.Mode=="Female"],na.rm=T),
                   fem.freq = length(Img.Age.Avg[Img.Gender.Mode=="Female"]), 
                   mal.age.avg=mean(Img.Age.Avg[Img.Gender.Mode=="Male"],na.rm=T),
                   mal.freq = length(Img.Age.Avg[Img.Gender.Mode=="Male"]))

google_gendered_agg_cat_match<-google_gendered_agg_cat_match[complete.cases(google_gendered_agg_cat_match),]

length(unique(google_gendered$face_id))
length(unique(google_gendered_agg_cat_match$Social.Category))

google_gendered_match_fem<-google_gendered_agg_cat_match %>% select(Social.Category, fem.age.avg)
colnames(google_gendered_match_fem)[2]<-"age"
google_gendered_match_fem$gender<-"Female"
google_gendered_match_male<-google_gendered_agg_cat_match %>% select(Social.Category, mal.age.avg)
colnames(google_gendered_match_male)[2]<-"age"
google_gendered_match_male$gender<-"Male"
google_gendered_match<-rbind(google_gendered_match_fem, google_gendered_match_male)

#Fig. 1C#
wiki_main_agg<-subset(data_main_agg, Data.Source=="Wikipedia")
wiki_main_agg_stats<-subset(data_main_agg_stats, Data.Source=="Wikipedia")

ggplot(wiki_main_agg,
       aes(x = Img.Age.Avg, fill=Img.Gender.Mode, color=Img.Gender.Mode, group=Img.Gender.Mode, linetype=Img.Gender.Mode)) + theme_bw() + 
  geom_density(lwd = 2, colour = "black", alpha = 0.7, bw = 0.4) +
  scale_fill_manual(values=c("goldenrod", "lightblue")) + 
  scale_linetype_manual(values=c("dotted", "solid")) + 
  xlab("Age of Face") + ggtitle("Wikipedia (Srinivasan et al. 2021)") + 
  theme(legend.text=element_text(size=60),
        legend.position=c(0.85,0.88),
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.5, angle=37, vjust=0.7),
        axis.text.y=element_text(size = 60, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits=c(-1,8), 
                     breaks=c(1,2,3,4,5,6,7),
                     labels=c("0-11", "12-17","18-24","25-34","35-54","55-74","+75")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  coord_cartesian(ylim=c(0,0.45)) + 
  geom_vline(xintercept = subset(wiki_main_agg_stats, Img.Gender.Mode=="Female")$Avg.Img.Age.Num, 
             color="goldenrod", size=4, alpha=0.8, linetype="dashed") + 
  geom_vline(xintercept = subset(wiki_main_agg_stats, Img.Gender.Mode=="Male")$Avg.Img.Age.Num, 
             color="blue", size=4, alpha=0.8)

#Fig 1C Stats#
t.test(subset(wiki_main_agg, Img.Gender.Mode=="Female")$Img.Age.Avg, 
       subset(wiki_main_agg, Img.Gender.Mode=="Male")$Img.Age.Avg)

#Fig 1C Stats (Matched by Category)#
wiki_main_cat_match<-wiki_main_agg %>% group_by(Social.Category) %>%
  dplyr::summarise(fem.age.avg=mean(Img.Age.Avg[Img.Gender.Mode=="Female"],na.rm=T),
                   fem.freq = length(Img.Age.Avg[Img.Gender.Mode=="Female"]), 
                   mal.age.avg=mean(Img.Age.Avg[Img.Gender.Mode=="Male"],na.rm=T),
                   mal.freq = length(Img.Age.Avg[Img.Gender.Mode=="Male"]))

wiki_main_cat_match<-wiki_main_cat_match[complete.cases(wiki_main_cat_match),]
t.test(wiki_main_cat_match$fem.age.avg, wiki_main_cat_match$mal.age.avg,paired=T)

################
#IP Replication#
####Figure 2####
################
IP_data<-read.csv("C:/Users/dougl/Desktop/comp-syn/algorithmic_bias/Data/RnR/Google_IP_test_final.csv",na.string ="")
IP_simp<-subset(IP_data, Img.Gender %in% c("Male", "Female"))
IP_data_clean<-subset(IP_data, Attention.Check & Img.Gender %in% c("Male","Female") & humface=="Yes")

IP_avg<-IP_data_clean %>% group_by(IP, Img.Gender) %>% 
  dplyr::summarise(avg_age=mean(Img.Age, na.rm=T), 
                   mode_age=getmode(Img.Age), 
                   median_age=getmode(Img.Age))

IP_data_clean %>% group_by(IP, Img.Gender) %>% 
  dplyr::summarise(avg_age=mean(Img.Age, na.rm=T), 
                   mode_age=getmode(Img.Age), 
                   median_age=getmode(Img.Age))

gen_age_stat<-IP_data_clean %>% group_by(IP) %>% 
  dplyr::summarise(t = t.test(Img.Age[Img.Gender=="Female"], Img.Age[Img.Gender=="Male"])$statistic, 
                   p = t.test(Img.Age[Img.Gender=="Female"], Img.Age[Img.Gender=="Male"])$p.value,
                   avgFemale=t.test(Img.Age[Img.Gender=="Female"], Img.Age[Img.Gender=="Male"])$estimate[1], 
                   avgMale=t.test(Img.Age[Img.Gender=="Female"], Img.Age[Img.Gender=="Male"])$estimate[2], 
                   diff=avgMale - avgFemale,
                   num_imgs=length(unique(face_id)))

IP_data$IP<-as.factor(IP_data$IP)
levels(IP_data$IP)<-c("Amsterdam","Bangalore","Frankfurt","Singapore", "Toronto")

#Fig. 2A#
IP_data_NY<-subset(subset(data_main, Data.Source=="Google"), Social.Category %in% unique(IP_data$Social.Category))

IP_data_NY_stats<-IP_data_NY %>% group_by(Img.Gender) %>% 
  dplyr::summarise(Avg.Img.Age=mean(Img.Age,na.rm=T))

ggplot(IP_data_NY, 
       aes(x = Img.Age, fill=Img.Gender, 
           color=Img.Gender, group=Img.Gender, 
           linetype=Img.Gender)) + 
  theme_bw() + geom_density(lwd = 2, colour = "black", alpha = 0.7, bw = 0.4) +
  scale_linetype_manual(values=c("dotted", "solid")) + 
  scale_fill_manual(values=c("goldenrod", "lightblue")) + 
  xlab("Age of Face") + ggtitle("New York") + 
  theme(legend.text=element_text(size=60),
        legend.position=c(0.85,0.88),
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.5, angle=37, vjust=0.7),
        axis.text.y=element_text(size = 60, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits=c(-1,8), 
                     breaks=c(1,2,3,4,5,6,7),
                     labels=c("0-11", "12-17","18-24","25-34","35-54","55-74","+75")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  coord_cartesian(ylim=c(0,0.45)) + 
  geom_vline(xintercept = subset(IP_data_NY_stats, Img.Gender=="Female")$Avg.Img.Age, 
             color="goldenrod", size=4, alpha=0.8, linetype="dashed") + 
  geom_vline(xintercept = subset(IP_data_NY_stats, Img.Gender=="Male")$Avg.Img.Age, 
             color="blue", size=4, alpha=0.8) 

t.test(subset(IP_data_NY, Img.Gender=="Female")$Img.Age, 
       subset(IP_data_NY, Img.Gender=="Male")$Img.Age)

#Fig. 2B#
ggplot(subset(IP_data_clean, IP == "Singapore"), 
       aes(x = Img.Age, fill=Img.Gender, 
           color=Img.Gender, group=Img.Gender, 
           linetype=Img.Gender)) + 
  theme_bw() + geom_density(lwd = 2, colour = "black", alpha = 0.7, bw = 0.38) +
  scale_linetype_manual(values=c("dotted", "solid")) + 
  scale_fill_manual(values=c("goldenrod", "lightblue")) + 
  xlab("Age of Face") + ggtitle("Singapore") + 
  theme(legend.text=element_text(size=60),
        legend.position=c(0.85,0.88),
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.5, angle=37, vjust=0.7),
        axis.text.y=element_text(size = 60, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits=c(-1,8), 
                     breaks=c(1,2,3,4,5,6,7),
                     labels=c("0-11", "12-17","18-24","25-34","35-54","55-74","+75")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  coord_cartesian(ylim=c(0,0.45)) + 
  geom_vline(xintercept = subset(gen_age_stat, IP=="Singapore")$avgFemale, 
             color="goldenrod", size=4, alpha=0.8, linetype="dashed") + 
  geom_vline(xintercept = subset(gen_age_stat, IP=="Singapore")$avgMale, 
             color="blue", size=4, alpha=0.8) 

t.test(subset(IP_data_clean, IP == "Singapore" & Img.Gender == "Female")$Img.Age, 
       subset(IP_data_clean, IP == "Singapore" & Img.Gender == "Male")$Img.Age)

#Fig. 2C#
ggplot(subset(IP_data_clean, IP == "Frankfurt"), 
       aes(x = Img.Age, fill=Img.Gender, 
           color=Img.Gender, group=Img.Gender, 
           linetype=Img.Gender)) + 
  theme_bw() + geom_density(lwd = 2, colour = "black", alpha = 0.7, bw = 0.45) +
  scale_linetype_manual(values=c("dotted", "solid")) + 
  scale_fill_manual(values=c("goldenrod", "lightblue")) + 
  xlab("Age of Face") + ggtitle("Frankfurt") + 
  theme(legend.text=element_text(size=60),
        legend.position=c(0.85,0.88),
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.5, angle=37, vjust=0.7),
        axis.text.y=element_text(size = 60, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits=c(-1,8), 
                     breaks=c(1,2,3,4,5,6,7),
                     labels=c("0-11", "12-17","18-24","25-34","35-54","55-74","+75")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  coord_cartesian(ylim=c(0,0.45)) + 
  geom_vline(xintercept = subset(gen_age_stat, IP=="Frankfurt")$avgFemale, 
             color="goldenrod", size=4, alpha=0.8, linetype="dashed") + 
  geom_vline(xintercept = subset(gen_age_stat, IP=="Frankfurt")$avgMale, 
             color="blue", size=4, alpha=0.8) 

t.test(subset(IP_data_clean, IP == "Frankfurt" & Img.Gender == "Female")$Img.Age, 
       subset(IP_data_clean, IP == "Frankfurt" & Img.Gender == "Male")$Img.Age)

#Fig. 2D#
ggplot(subset(IP_data_clean, IP == "Bangalore"), 
       aes(x = Img.Age, fill=Img.Gender, 
           color=Img.Gender, group=Img.Gender, 
           linetype=Img.Gender)) + 
  theme_bw() + geom_density(lwd = 2, colour = "black", alpha = 0.7, bw = 0.33) +
  scale_linetype_manual(values=c("dotted", "solid")) + 
  scale_fill_manual(values=c("goldenrod", "lightblue")) + 
  xlab("Age of Face") + ggtitle("Bangalore") + 
  theme(legend.text=element_text(size=60),
        legend.position=c(0.85,0.88),
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.5, angle=37, vjust=0.7),
        axis.text.y=element_text(size = 60, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits=c(-1,8), 
                     breaks=c(1,2,3,4,5,6,7),
                     labels=c("0-11", "12-17","18-24","25-34","35-54","55-74","+75")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  coord_cartesian(ylim=c(0,0.45)) + 
  geom_vline(xintercept = subset(gen_age_stat, IP=="Bangalore")$avgFemale, 
             color="goldenrod", size=4, alpha=0.8, linetype="dashed") + 
  geom_vline(xintercept = subset(gen_age_stat, IP=="Bangalore")$avgMale, 
             color="blue", size=4, alpha=0.8) 

t.test(subset(IP_data_clean, IP == "Bangalore" & Img.Gender == "Female")$Img.Age, 
       subset(IP_data_clean, IP == "Bangalore" & Img.Gender == "Male")$Img.Age)

#Fig. 2E#
ggplot(subset(IP_data_clean, IP == "Toronto"), 
       aes(x = Img.Age, fill=Img.Gender, 
           color=Img.Gender, group=Img.Gender, 
           linetype=Img.Gender)) + 
  theme_bw() + geom_density(lwd = 2, colour = "black", alpha = 0.7, bw = 0.4) +
  scale_linetype_manual(values=c("dotted", "solid")) + 
  scale_fill_manual(values=c("goldenrod", "lightblue")) + 
  xlab("Age of Face") + ggtitle("Toronto") + 
  theme(legend.text=element_text(size=60),
        legend.position=c(0.85,0.88),
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.5, angle=37, vjust=0.7),
        axis.text.y=element_text(size = 60, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits=c(-1,8), 
                     breaks=c(1,2,3,4,5,6,7),
                     labels=c("0-11", "12-17","18-24","25-34","35-54","55-74","+75")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  coord_cartesian(ylim=c(0,0.45)) + 
  geom_vline(xintercept = subset(gen_age_stat, IP=="Toronto")$avgFemale, 
             color="goldenrod", size=4, alpha=0.8, linetype="dashed") + 
  geom_vline(xintercept = subset(gen_age_stat, IP=="Toronto")$avgMale, 
             color="blue", size=4, alpha=0.8) 

t.test(subset(IP_data_clean, IP == "Toronto" & Img.Gender == "Female")$Img.Age, 
       subset(IP_data_clean, IP == "Toronto" & Img.Gender == "Male")$Img.Age)

#Fig. 2F#
ggplot(subset(IP_data_clean, IP == "Amsterdam"), 
       aes(x = Img.Age, fill=Img.Gender, 
           color=Img.Gender, group=Img.Gender, 
           linetype=Img.Gender)) + 
  theme_bw() + geom_density(lwd = 2, colour = "black", alpha = 0.7, bw = 0.38) +
  scale_linetype_manual(values=c("dotted", "solid")) + 
  scale_fill_manual(values=c("goldenrod", "lightblue")) + 
  xlab("Age of Face") + ggtitle("Amsterdam") + 
  theme(legend.text=element_text(size=60),
        legend.position=c(0.85,0.88),
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.5, angle=37, vjust=0.7),
        axis.text.y=element_text(size = 60, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits=c(-1,8), 
                     breaks=c(1,2,3,4,5,6,7),
                     labels=c("0-11", "12-17","18-24","25-34","35-54","55-74","+75")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  coord_cartesian(ylim=c(0,0.45)) + 
  geom_vline(xintercept = subset(gen_age_stat, IP=="Amsterdam")$avgFemale, 
             color="goldenrod", size=4, alpha=0.8, linetype="dashed") + 
  geom_vline(xintercept = subset(gen_age_stat, IP=="Amsterdam")$avgMale, 
             color="blue", size=4, alpha=0.8) 

t.test(subset(IP_data_clean, IP == "Amsterdam" & Img.Gender == "Female")$Img.Age, 
       subset(IP_data_clean, IP == "Amsterdam" & Img.Gender == "Male")$Img.Age)

#################################################
##Replication with Ground-truth Classifications##
##################Figure 3#######################
#################################################

#Fig. 3A#
ggplot(IMDBceleb, aes(x = age, fill=gender_cat, alpha=gender_cat, linetype=gender_cat)) +
  geom_density(size=1.4) + theme_bw() + 
  scale_alpha_manual(values=c(0.7,0.7)) + 
  scale_linetype_manual(values=c("dotted", "solid")) + 
  scale_fill_manual(values=c("goldenrod", "lightblue")) + 
  ylab("Density") + xlab("Age of Face") + ggtitle("IMDb (Rothe et al. 2018)") +
  theme(legend.text=element_text(size=60),
        legend.position=c(0.7,0.85),
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_blank(),
        axis.text.x=element_text(size = 60, hjust = 0.5),
        axis.text.y=element_text(size = 60, hjust = 0.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  scale_x_continuous(limits=c(0,100), breaks=c(0,10,20,30,40,50,60,70,80,90,100)) + 
  geom_vline(xintercept = mean(subset(IMDBceleb, gender_cat=="Female")$age, na.rm=T), 
             color="goldenrod", size=4, alpha=0.8, linetype="dashed") + 
  geom_vline(xintercept = mean(subset(IMDBceleb, gender_cat=="Male")$age, na.rm=T), color="blue", size=4, alpha=0.8)

#Fig. 3A Stats#
imdb_female=subset(IMDBceleb, gender==0)
imdb_male=subset(IMDBceleb, gender==1)
t.test(imdb_female$age, imdb_male$age)

#Fig. 3B#
ggplot(wikiceleb, aes(x = age, fill=gender_cat, alpha=gender_cat, linetype=gender_cat)) +
  geom_density(size=1.4) + theme_bw() + 
  scale_alpha_manual(values=c(0.7,0.7)) + 
  scale_linetype_manual(values=c("dotted", "solid")) + 
  scale_fill_manual(values=c("goldenrod", "lightblue")) + 
  ylab("Density") + xlab("Age of Face") + ggtitle("Wikipedia (Rothe et al. 2018)") + 
  theme(legend.text=element_text(size=60),
        legend.position=c(0.7,0.85),
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_blank(),
        axis.text.x=element_text(size = 60, hjust = 0.5),
        axis.text.y=element_text(size = 60, hjust = 0.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  scale_x_continuous(limits=c(0,100), breaks=c(0,10,20,30,40,50,60,70,80,90,100)) + 
  geom_vline(xintercept = mean(subset(IMDBceleb, gender_cat=="Female")$age, na.rm=T), 
             color="goldenrod", size=4, alpha=0.8, linetype="dashed") + 
  geom_vline(xintercept = mean(subset(IMDBceleb, gender_cat=="Male")$age, na.rm=T), color="blue", size=4, alpha=0.8)

#Fig. 3B Stats#
wiki_female=subset(wikiceleb, gender==0)
wiki_male=subset(wikiceleb, gender==1)
t.test(wiki_female$age, wiki_male$age)

#Fig. 3C#
ggplot(CADC_comp, aes(x = age, fill=gender, alpha=gender, linetype=gender)) +
  geom_density(size=1.4) + theme_bw() + 
  scale_alpha_manual(values=c(0.7,0.7)) + 
  scale_linetype_manual(values=c("dotted", "solid")) + 
  scale_fill_manual(values=c("goldenrod", "lightblue")) + 
  ylab("Density") + xlab("Age of Face") + ggtitle("Google (Chen et al. 2014)") +
  theme(legend.text=element_text(size=60),
        legend.position=c(0.12,0.92),
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_blank(),
        axis.text.x=element_text(size = 60, hjust = 0.5),
        axis.text.y=element_text(size = 60, hjust = 0.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  scale_x_continuous(limits=c(7,73), breaks=c(0,10,20,30,40,50,60,70,80,90,100)) + 
  geom_vline(xintercept = mean(subset(CADC_comp, gender=="Female")$age, na.rm=T), 
             color="goldenrod", size=4, alpha=0.8, linetype="dashed") + 
  geom_vline(xintercept = mean(subset(CADC_comp, gender=="Male")$age, na.rm=T), color="blue", size=4, alpha=0.8)

#Fig. 3C Stats#
t.test(subset(CADC_comp, gender=="Male")$age,subset(CADC_comp, gender=="Female")$age)

########################
#Supplementary Analyses#
########################

##############################
##MAIN RESULTS WITH RAW DATA##
##############################
t.test(subset(subset(data_raw, Data.Source=="Google"), Img.Gender=="Female")$Img.Age, 
       subset(subset(data_raw, Data.Source=="Google"), Img.Gender=="Male")$Img.Age)

google_raw_agg<- subset(data_raw, Data.Source=="Google") %>% 
  group_by(Social.Category, searchDEMO, face_id, image_id) %>% 
  dplyr::summarise(Img.Gender.Mode=getmode(Img.Gender), 
                   Img.Age.mode=getmode(Img.Age), 
                   Img.Age.avg=mean(Img.Age,na.rm=T))

t.test(subset(google_raw_agg, Img.Gender.Mode=="Female")$Img.Age.mode, 
       subset(google_raw_agg, Img.Gender.Mode=="Male")$Img.Age.mode)

######################################
##Controlling for Coder Demographics##
######################################
demo_all<-unique(data_clean %>% select(WorkerId, coder_ideo, coder_gender, coder_race, coder_age, coder_inc, coder_edu))
length(unique(demo_all$WorkerId))

table(demo_all$coder_gender)/sum(table(demo_all$coder_gender))
table(demo_all$coder_race)/sum(table(demo_all$coder_race))
table(demo_all$coder_age)/sum(table(demo_all$coder_age))
table(demo_all$coder_ideo)/sum(table(demo_all$coder_ideo))
table(demo_all$coder_inc)/sum(table(demo_all$coder_inc))
table(demo_all$coder_edu)/sum(table(demo_all$coder_edu))

fem_demo_all<-subset(demo_all, coder_gender=="Female")
table(fem_demo_all$coder_age)/sum(table(fem_demo_all$coder_age))
male_demo_all<-subset(demo_all, coder_gender=="Male")
table(demo_all$coder_age)/sum(table(demo_all$coder_age))

#Run Models
memory.limit(60000)
mod_catFEs<-lm(Img.Age ~ Img.Gender + Data.Source + Social.Category, data = data_main)
summary(mod_catFEs)
mod_catFEs_vcov <- cluster.vcov(mod_catFEs, data_main$image_id)
coeftest(mod_catFEs, mod_catFEs_vcov)

mod_demo<-lm(Img.Age ~ Img.Gender + Data.Source + coder_gender + coder_age + coder_race + 
               coder_ideo + coder_inc + Social.Category, data = data_main)

summary(mod_demo)
mod_demo_vcov <- cluster.vcov(mod_demo, data_main$image_id)
coeftest(mod_demo, mod_demo_vcov)

mod_demo_binary_full<-lm(Img.Age ~ Img.Gender + Data.Source, data = data_main)
summary(mod_demo_binary_full)
mod_demo_binary_full_vcov <- cluster.vcov(mod_demo_binary_full, data_main$Social.Category)
coeftest(mod_demo_binary_full, mod_demo_vcov)

mod_demo_full<-lm(Img.Age ~ Img.Gender + Data.Source + coder_gender + coder_age + coder_race + 
                    coder_ideo + coder_inc, data = data_main)
summary(mod_demo_full)
mod_demo_full_vcov <- cluster.vcov(mod_demo_full, data_main$Social.Category)
coeftest(mod_demo_full, mod_demo_vcov)

############################
#Controlling for coders FEs#
############################
mod_coderFEs<-lm(Img.Age ~ Img.Gender + Data.Source + WorkerId, data = data_binary)
summary(mod_coderFEs)

mod_demo_binary_full_vcov <- cluster.vcov(mod_coderFEs, data_binary$Social.Category)
coeftest(mod_demo_binary_full, mod_demo_vcov)

#######################################
#Controlling for inter-coder agreement#
#######################################
data_all_num_workers<-data_binary %>% 
  group_by(face_id, Social.Category, Data.Source, searchDEMO) %>% 
  dplyr::summarise(nworkers=length(unique(WorkerId)))

data_all_freq_gender_binary<-data_binary %>% 
  group_by(face_id, Img.Gender, Social.Category, Data.Source, searchDEMO) %>% 
  dplyr::summarise(n_uni_worker=length(unique(WorkerId))) %>% 
  group_by(face_id, Social.Category, Data.Source, searchDEMO) %>% 
  top_n(1, wt=n_uni_worker)

data_all_freq_gender_full<-merge(data_all_num_workers, 
                                 data_all_freq_gender_binary, 
                                 by=c("face_id", "Social.Category", 'Data.Source', 'searchDEMO'))

data_all_freq_gender_full$rate_agree<-data_all_freq_gender_full$n_uni_worker/data_all_freq_gender_full$nworkers

mean(data_all_freq_gender_full$rate_agree)
sum(data_all_freq_gender_full$rate_agree==1)/nrow(data_all_freq_gender_full)

#age classification
data_all_freq_age_binary<-data_binary %>% 
  group_by(face_id, Img.Age, Social.Category, Data.Source, searchDEMO) %>% 
  dplyr::summarise(n_uni_worker=length(unique(WorkerId))) %>% 
  group_by(face_id, Social.Category, Data.Source, searchDEMO) %>% 
  top_n(1, wt=n_uni_worker)

data_all_freq_age_full<-merge(data_all_num_workers, data_all_freq_age_binary, by=c("face_id", "Social.Category", 'Data.Source', 'searchDEMO'))
data_all_freq_age_full$rate_agree<-data_all_freq_age_full$n_uni_worker/data_all_freq_age_full$nworkers

mean(data_all_freq_age_full$rate_agree)
sum(data_all_freq_age_full$rate_agree==1)/nrow(data_all_freq_age_full)

data_all_freq_age_dist<-data_binary %>% 
  group_by(face_id, Social.Category, Data.Source, searchDEMO) %>% 
  dplyr::summarise(avg_age_dist=mean(diff(Img.Age), na.rm=T)) 

data_all_freq_age_dist$abs_dist<-abs(data_all_freq_age_dist$avg_age_dist)
mean(data_all_freq_age_dist$abs_dist, na.rm=T)

data_all_freq_age_dist_binary<-data_all_freq_age_dist %>% dplyr::select(face_id, avg_age_dist, abs_dist)
data_all_freq_age_dist_binary<-data_all_freq_age_dist_binary[complete.cases(data_all_freq_age_dist_binary),]
sum(data_all_freq_age_dist_binary$abs_dist<=1)/nrow(data_all_freq_age_dist_binary)

#merge to map with gender and age agree
data_all_freq_gender_full_binary<-data_all_freq_gender_full %>% 
  group_by(face_id, Social.Category, Data.Source, searchDEMO, nworkers) %>% 
  top_n(1, wt=rate_agree)

data_all_freq_gender_full_binary<-data_all_freq_gender_full_binary %>% dplyr::select(-Img.Gender)
data_all_freq_gender_full_binary<-data_all_freq_gender_full_binary %>% 
  group_by(face_id, Social.Category, Data.Source,searchDEMO, nworkers) %>% 
  sample_n(1)

data_clean_agg_m<-merge(data_binary_agg,
                        data_all_freq_gender_full_binary, 
                        by=c("face_id","Social.Category","Data.Source","searchDEMO"))
names(data_clean_agg_m)[11]<-"rate_agree_gender"

data_all_freq_age_full_binary<-data_all_freq_age_full %>% 
  group_by(face_id, Social.Category, Data.Source,searchDEMO, nworkers) %>% 
  top_n(1, wt=rate_agree)

data_all_freq_age_full_binary<-data_all_freq_age_full_binary %>% dplyr::select(-Img.Age)
data_all_freq_age_full_binary<-data_all_freq_age_full_binary %>% group_by(face_id, Social.Category, Data.Source,searchDEMO, nworkers) %>% sample_n(1)

data_clean_agg_m<-merge(data_clean_agg_m, data_all_freq_age_full_binary, 
                            by=c("face_id","Social.Category",
                                 "Data.Source","searchDEMO"))
names(data_clean_agg_m)[14]<-"rate_agree_age"

#Run Models Controlling for Intercoder Agreement
coder_agree_age<-lm(Img.Age.Avg ~ Img.Gender.Mode + Data.Source + rate_agree_age + rate_agree_gender, data=data_clean_agg_m)
summary(coder_agree_age)
coder_agree_age_vcov <- cluster.vcov(coder_agree_age, data_clean_agg_m$face_id)
coeftest(coder_agree_age, coder_agree_age_vcov)

data_clean_agg_m$Img.Gender.Binary<-as.factor(data_clean_agg_m$Img.Gender.Mode)
levels(data_clean_agg_m$Img.Gender.Binary)<-c(0,1)

coder_agree_gender=lrm(Img.Gender.Binary ~ Img.Age.Avg + Data.Source + rate_agree_age + rate_agree_gender, x=T, y=T, data=data_clean_agg_m)
robcov(coder_agree_gender, cluster=data_clean_agg_m$face_id)
exp(coef(coder_agree_gender))

###################################
#Robustness to Unanimous Agreement#
###################################
data_all_unanimous<-subset(data_clean_agg_m, rate_agree_gender==1 & rate_agree_age==1)

google_unanimous<-subset(data_all_unanimous, Data.Source=="Google")

ggplot(google_unanimous, aes(x = Img.Age.Avg, fill=Img.Gender.Mode, color=Img.Gender.Mode, group=Img.Gender.Mode, linetype=Img.Gender.Mode)) + 
  theme_bw() + geom_density(lwd = 2, colour = "black", alpha = 0.7, bw=0.7) +
  scale_linetype_manual(values=c("dotted", "solid")) + scale_fill_manual(values=c("goldenrod", "lightblue")) + 
  xlab("Age of Face") + ggtitle("Google") + 
  theme(legend.text=element_text(size=60),
        legend.position=c(0.17,0.88),
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.5, angle=37, vjust=0.7),
        axis.text.y=element_text(size = 60, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits=c(-1,8), breaks=c(1,2,3,4,5,6,7),labels=c("0-11", "12-17","18-24","25-34","35-54","55-74","+75")) + 
  geom_vline(xintercept = mean(subset(google_unanimous, Img.Gender.Mode=="Male")$Img.Age.Avg), 
             color="blue", size=2, alpha=1, linetype="solid") + 
  geom_vline(xintercept = mean(subset(google_unanimous, Img.Gender.Mode=="Female")$Img.Age.Avg),
             color="goldenrod", size=2, alpha=1) 

t.test(subset(google_unanimous, Img.Gender.Mode=="Female")$Img.Age.Avg, 
       subset(google_unanimous, Img.Gender.Mode=="Male")$Img.Age.Avg)

wiki_unanimous<-subset(data_all_unanimous, Data.Source=="Wikipedia")

ggplot(wiki_unanimous, aes(x = Img.Age.Avg, fill=Img.Gender.Mode, color=Img.Gender.Mode, group=Img.Gender.Mode, linetype=Img.Gender.Mode)) + 
  theme_bw() + geom_density(lwd = 2, colour = "black", alpha = 0.7, bw=0.7) +
  scale_linetype_manual(values=c("dotted", "solid")) + scale_fill_manual(values=c("goldenrod", "lightblue")) + 
  xlab("Age of Face") + ggtitle("Wikipedia") + 
  theme(legend.text=element_text(size=60),
        legend.position=c(0.17,0.88),
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.5, angle=37, vjust=0.7),
        axis.text.y=element_text(size = 60, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits=c(-1,8), breaks=c(1,2,3,4,5,6,7),labels=c("0-11", "12-17","18-24","25-34","35-54","55-74","+75")) + 
  geom_vline(xintercept = mean(subset(wiki_unanimous, Img.Gender.Mode=="Male")$Img.Age.Avg), 
             color="blue", size=2, alpha=1, linetype="solid") + 
  geom_vline(xintercept = mean(subset(wiki_unanimous, Img.Gender.Mode=="Female")$Img.Age.Avg),
             color="goldenrod", size=2, alpha=1) 

t.test(subset(wiki_unanimous, Img.Gender.Mode=="Female")$Img.Age.Avg, 
       subset(wiki_unanimous, Img.Gender.Mode=="Male")$Img.Age.Avg)

data_all_NONunanimous<-subset(data_clean_agg_m, rate_agree_gender<1 & rate_agree_age<1)
data_all_clean_NONunanimous<-subset(data_clean, face_id %in% unique(data_all_NONunanimous$face_id))

table(data_all_clean_NONunanimous$Img.Age)/sum(table(data_all_clean_NONunanimous$Img.Age))
table(data_all_clean_NONunanimous$Img.Gender)/sum(table(data_all_clean_NONunanimous$Img.Gender))

######################################################
#Controlling for images that repeated across searches#
######################################################

#repeat images (Google)#
google_clean<-subset(data_clean, Data.Source=="Google")
google_clean$demo_cat<-paste(google_clean$searchDEMO, google_clean$Social.Category, sep="_")
google_repeat_check<-google_clean %>% group_by(face_id) %>% dplyr::summarise(numCats=length(unique(demo_cat)))
table(google_repeat_check$numCats)/sum(table(google_repeat_check$numCats))
google_repeat_check_norep<-subset(google_repeat_check, numCats==1)
google_clean_norep<-subset(google_clean, face_id %in% google_repeat_check_norep$face_id)
google_clean_norep_main<-subset(google_clean_norep, !Img.Gender %in% c("Non-binary"))

google_clean_norep_main_agg<- google_clean_norep_main %>% group_by(Data.Source, Social.Category, searchDEMO, face_id, image_id) %>% 
  dplyr::summarise(Img.Gender.Mode=getmode(Img.Gender), 
                   Img.Age.mode=getmode(Img.Age), 
                   Img.Age.avg=mean(Img.Age,na.rm=T))

ggplot(google_clean_norep_main_agg, aes(x = Img.Age.avg, fill=Img.Gender.Mode, color=Img.Gender.Mode, group=Img.Gender.Mode, linetype=Img.Gender.Mode)) + 
  theme_bw() + geom_density(lwd = 2, colour = "black", alpha = 0.7, bw=0.3) +
  scale_linetype_manual(values=c("dotted", "solid")) + scale_fill_manual(values=c("goldenrod", "lightblue")) + 
  xlab("Age of Face") + ggtitle("Google") + 
  theme(legend.text=element_text(size=60),
        legend.position=c(0.25,0.7),
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.5, angle=37, vjust=0.7),
        axis.text.y=element_text(size = 60, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits=c(-1,8), breaks=c(1,2,3,4,5,6,7),labels=c("0-11", "12-17","18-24","25-34","35-54","55-74","+75")) + 
  geom_vline(xintercept = mean(subset(google_clean_norep_main_agg, Img.Gender.Mode=="Male")$Img.Age.avg), 
             color="blue", size=2, alpha=1, linetype="solid") + 
  geom_vline(xintercept = mean(subset(google_clean_norep_main_agg, Img.Gender.Mode=="Female")$Img.Age.avg),
             color="goldenrod", size=2, alpha=1) 

t.test(subset(google_clean_norep_main_agg, Img.Gender.Mode=="Female")$Img.Age.mode, 
       subset(google_clean_norep_main_agg, Img.Gender.Mode=="Male")$Img.Age.mode)

google_clean_rep<-subset(google_clean, !face_id %in% google_repeat_check_norep$face_id)
google_clean_rep_main_agg<- google_clean_rep %>% group_by(source, Social.Category, searchDEMO, face_id, image_id, Gendered.Category) %>% 
  dplyr::summarise(Img.Gender.Mode=getmode(Img.Gender), 
                   Img.Age.mode=getmode(Img.Age), 
                   Img.Age.avg=mean(Img.Age,na.rm=T))

t.test(subset(google_clean_rep_main_agg, Img.Gender.Mode=="Female")$Img.Age.mode, 
       subset(google_clean_rep_main_agg, Img.Gender.Mode=="Male")$Img.Age.mode)

#repeat images (Wiki)#
wiki_repeat_check<-wiki_main %>% group_by(face_id) %>% dplyr::summarise(numCats=length(unique(Social.Category)))
table(wiki_repeat_check$numCats)/sum(table(wiki_repeat_check$numCats))
wiki_repeat_check_norep<-subset(wiki_repeat_check, numCats==1)
wiki_clean_norep<-subset(wiki_main, face_id %in% wiki_repeat_check_norep$face_id)
wiki_clean_norep_main<-subset(wiki_clean_norep, !Img.Gender %in% c("Non-binary"))

wiki_clean_norep_main_agg<- wiki_clean_norep_main %>% 
  group_by(Data.Source, Social.Category, face_id, image_id) %>% 
  dplyr::summarise(Img.Gender.Mode=getmode(Img.Gender), 
                   Img.Age.mode=getmode(Img.Age), 
                   Img.Age.avg=mean(Img.Age,na.rm=T))

ggplot(wiki_clean_norep_main_agg, aes(x = Img.Age.avg, fill=Img.Gender.Mode, color=Img.Gender.Mode, group=Img.Gender.Mode, linetype=Img.Gender.Mode)) + 
  theme_bw() + geom_density(lwd = 2, colour = "black", alpha = 0.7, bw=0.7) +
  scale_linetype_manual(values=c("dotted", "solid")) + scale_fill_manual(values=c("goldenrod", "lightblue")) + 
  xlab("Age of Face") + ggtitle("Wikipedia") + 
  theme(legend.text=element_text(size=60),
        legend.position=c(0.2,0.7),
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.5, angle=37, vjust=0.7),
        axis.text.y=element_text(size = 60, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits=c(-1,8), breaks=c(1,2,3,4,5,6,7),labels=c("0-11", "12-17","18-24","25-34","35-54","55-74","+75")) + 
  geom_vline(xintercept = mean(subset(wiki_clean_norep_main_agg, Img.Gender.Mode=="Male")$Img.Age.avg), 
             color="blue", size=2, alpha=1, linetype="solid") + 
  geom_vline(xintercept = mean(subset(wiki_clean_norep_main_agg, Img.Gender.Mode=="Female")$Img.Age.avg),
             color="goldenrod", size=2, alpha=1) 

t.test(subset(wiki_clean_norep_main_agg, Img.Gender.Mode=="Female")$Img.Age.avg, 
       subset(wiki_clean_norep_main_agg, Img.Gender.Mode=="Male")$Img.Age.avg)

wiki_main_rep<-subset(wiki_main, !face_id %in% wiki_clean_norep$face_id)
wiki_main_rep_agg<- wiki_main_rep %>% group_by(source, Social.Category, face_id, image_id) %>% 
  dplyr::summarise(Img.Gender.Mode=getmode(Img.Gender), 
                   Img.Age.mode=getmode(Img.Age), 
                   Img.Age.avg=mean(Img.Age,na.rm=T))

t.test(subset(wiki_main_rep_agg, Img.Gender.Mode=="Female")$Img.Age.mode, 
       subset(wiki_main_rep_agg, Img.Gender.Mode=="Male")$Img.Age.mode)

##################################################################
#####Controlling for Number of Faces in Each Image################
##################################################################

#num faces (Google)#
data_binary_wNumFaces<-data_binary %>% group_by(Data.Source, image_id) %>% dplyr::summarise(num_faces = length(unique(face_id)))
data_binary_wNumFaces_full<-merge(data_binary, data_binary_wNumFaces, by=c("Data.Source", "image_id"))

mod_data_all_wNumFaces<-lm(Img.Age ~ Img.Gender + Data.Source + num_faces, data = data_binary_wNumFaces_full)
summary(mod_data_all_wNumFaces)
mod_data_all_wNumFaces_vcov <- cluster.vcov(mod_data_all_wNumFaces, data_binary_wNumFaces_full$image_id)
coeftest(mod_data_all_wNumFaces, mod_data_all_wNumFaces_vcov)

############################################
#Controlling for frequency in Google Trends#
############################################
google_trends_freq<-read.csv("C:/Users/dougl/Desktop/comp-syn/algorithmic_bias/Data/NHB/Google_Search_Frequency_data.csv")
google_main_wfreq<-merge(subset(data_main, Data.Source=="Google"), 
                         google_trends_freq, by=c("Social.Category"))

google_freq_mod<-lm(Img.Age ~ Img.Gender + Google.Img.US.Search.Freq, data=google_main_wfreq)
summary(google_freq_mod)
google_freq_vcov_category <- cluster.vcov(google_freq_mod, google_main_wfreq$Social.Category)
coeftest(google_freq_mod, google_freq_vcov_category)

google_freq_vcov_coder <- cluster.vcov(google_freq_mod, google_main_wfreq$WorkerId)
coeftest(google_freq_mod, google_freq_vcov_coder)

###############################
#Robustness aggregation method#
###############################
t.test(subset(google_main_agg, Img.Gender.Mode == "Female")$Img.Age.avg, 
       subset(google_main_agg, Img.Gender.Mode == "Male")$Img.Age.avg)

t.test(subset(google_gendered, Img.Gender.Mode == "Female")$Img.Age.avg, 
       subset(google_gendered, Img.Gender.Mode == "Male")$Img.Age.avg)

t.test(subset(wiki_main_agg, Img.Gender.Mode == "Female")$Img.Age.avg, 
       subset(wiki_main_agg, Img.Gender.Mode == "Male")$Img.Age.avg)

##########################################
#Robustness to examining uncropped images#
##########################################
uncropped<-read.csv("C:/Users/dougl/Desktop/comp-syn/algorithmic_bias/Data/NHB/Google_uncropped.csv")
uncropped_clean<-subset(uncropped, humface=="Yes" & Attention.Check & Img.Gender %in% c("Male", "Female"))

uncropped_stats<-uncropped_clean %>% group_by(Img.Gender) %>% 
  dplyr::summarise(avg_age=mean(Img.Age), modal_age=getmode(Img.Age))

ggplot(uncropped_clean, aes(x = Img.Age, fill=Img.Gender, color=Img.Gender, group=Img.Gender, linetype=Img.Gender)) + 
  geom_density(lwd = 2, colour = "black", alpha = 0.7, bw = 0.7) + theme_bw() + 
  scale_fill_manual(values=c("goldenrod", "lightblue")) + 
  scale_linetype_manual(values=c("dotted", "solid")) + 
  xlab("Age of Face") + ggtitle("Google (Uncropped Images)") + 
  theme(legend.text=element_text(size=60),
        legend.position=c(0.85,0.88),
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.5, angle=37, vjust=0.7),
        axis.text.y=element_text(size = 60, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits=c(-2,9.5), 
                     breaks=c(1,2,3,4,5,6,7),
                     labels=c("0-11", "12-17","18-24","25-34","35-54","55-74","+75")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  geom_vline(xintercept = subset(uncropped_stats, Img.Gender=="Female")$modal_age, 
             color="goldenrod", size=4, alpha=0.8, linetype="dashed") + 
  geom_vline(xintercept = subset(uncropped_stats, Img.Gender=="Male")$modal_age, 
             color="blue", size=4, alpha=0.8)

t.test(subset(uncropped_clean, Img.Gender=="Female")$Img.Age, subset(uncropped_clean, Img.Gender=="Male")$Img.Age)

#########################################
#Classification Accuracy Validation Task#
#########################################
validation_dt<-read.csv("C:/Users/dougl/Desktop/comp-syn/algorithmic_bias/Data/RnR/Final_Submit/coder_validation_task.csv")
validation_dt_age<-subset(validation_dt, feature=="age")
validation_dt_age$age<-as.factor(validation_dt_age$response)
validation_dt_age$age_num<-validation_dt_age$age
levels(validation_dt_age$age_num)<-c(1,2,3,4,5,6,7)
validation_dt_age$age_num<-as.numeric(as.character(validation_dt_age$age_num))

#rate accuracy age
validation_dt_age$age_match<-validation_dt_age$age==validation_dt_age$real_age
table(validation_dt_age$age_match)/sum(table(validation_dt_age$age_match))

#avg dist of age classification
validation_dt_age$num_diff<-as.numeric(validation_dt_age$real_age_num) - as.numeric(validation_dt_age$age_num)
validation_dt_age$mod_acc_r1<-abs(validation_dt_age$num_diff) <= 1
table(validation_dt_age$mod_acc_r1)/sum(table(validation_dt_age$mod_acc_r1))

#rate accuracy gender
validation_dt_gender<-subset(validation_dt, feature=="gender")
colnames(validation_dt_gender)[2]<-"Guess.Gender"
validation_dt_gender$gender_match<-validation_dt_gender$Guess.Gender==validation_dt_gender$real_gender
table(validation_dt_gender$gender_match)/sum(table(validation_dt_gender$gender_match))

##Familiar##
validation_dt_familiar<-subset(validation_dt, feature=="fam")
colnames(validation_dt_familiar)[2]<-"familiar"
table(validation_dt_familiar$familiar)/sum(table(validation_dt_familiar$familiar))

validation_dt_familiar_agg<-validation_dt_familiar %>% 
  group_by(img_id) %>% 
  dplyr::summarise(propFAM=sum(familiar=="Yes")/length(familiar))

mean(validation_dt_familiar_agg$propFAM)
subset(validation_dt_familiar_agg, propFAM>0.3)
sum(validation_dt_familiar_agg$propFAM<0.1)/nrow(validation_dt_familiar_agg)
hist(validation_dt_familiar_agg$propFAM, breaks=40)

validation_dt_familiar_agg$gender<-sapply(1:nrow(validation_dt_familiar_agg), 
                                          function(x) strsplit(validation_dt_familiar_agg[x,]$img_id, "_")[[1]][2])

validation_dt_familiar_agg %>% group_by(gender) %>% dplyr::summarise(propFAM=mean(propFAM))
wilcox.test(subset(validation_dt_familiar_agg, gender=="f")$propFAM, subset(validation_dt_familiar_agg, gender=="m")$propFAM)

validation_dt_familiar_agg$age<-sapply(1:nrow(validation_dt_familiar_agg), 
                                       function(x) strsplit(validation_dt_familiar_agg[x,]$img_id, "_")[[1]][3])
validation_dt_familiar_agg$age<-as.numeric(as.factor(validation_dt_familiar_agg$age))
validation_dt_familiar_agg %>% group_by(age) %>% dplyr::summarise(propFAM=mean(propFAM))
jonckheere.test(validation_dt_familiar_agg$propFAM, validation_dt_familiar_agg$age)

validation_dt_full<-merge(validation_dt_age, validation_dt_gender, by=c("SubjID", "img_id", "real_age", "real_age_num", "real_gender"))
validation_dt_full<-merge(validation_dt_full, validation_dt_familiar, by=c("SubjID", "img_id", "real_age", "real_age_num","real_gender"))
validation_dt_full_clean<-subset(validation_dt_full, familiar!="Yes")

table(validation_dt_full_clean$age_match)/sum(table(validation_dt_full_clean$age_match))
table(validation_dt_full_clean$mod_acc_r1)/sum(table(validation_dt_full_clean$mod_acc_r1))
table(validation_dt_full_clean$gender_match)/sum(table(validation_dt_full_clean$gender_match))

validation_dt_f<-subset(validation_dt_full_clean, real_gender=="Female")
validation_dt_m<-subset(validation_dt_full_clean, real_gender=="Male")
wilcox.test(validation_dt_f$real_age_num, validation_dt_m$real_age_num)

#run model
validation_dt_full_binary<-subset(validation_dt_full, Guess.Gender!="Non-binary")
mod_within_age<-lm(age_num ~ real_age + Guess.Gender + familiar + SubjID, data=validation_dt_full_binary)
summary(mod_within_age)
user_vcov <- cluster.vcov(mod_within_age, validation_dt_full_binary$img_id)
coeftest(mod_within_age, user_vcov)

#evaluate accuracy by gender of the face
validation_dt_gender_agg<-validation_dt_full_clean %>% group_by(real_gender) %>% 
  dplyr::summarise(propAgecorr=sum(age_match)/length(age_match), 
                   num_age_match = sum(age_match), 
                   n_age_match = length(age_match),
                   avg_age_num = mean(as.numeric(age_num)))

prop.test(x = c(subset(validation_dt_gender_agg, real_gender=="Female")$num_age_match, 
                subset(validation_dt_gender_agg, real_gender=="Male")$num_age_match), 
          n = c(subset(validation_dt_gender_agg, real_gender=="Female")$n_age_match, 
                subset(validation_dt_gender_agg, real_gender=="Male")$n_age_match))

#################################################################
###############false negative simulation#########################
#######Evaluating effects of adding "biased" missing images######
##########where bias is opposite to our main theory;############# 
#ie, where missing faces skew toward young males and old females#
#################################################################

#false negative simulation (Google)
google_clean_agg_binary<-subset(google_clean_agg, !Img.Gender.Mode %in% c("Non-binary"))
numfaces<-length(unique(google_clean_agg_binary$face_id))
male_to_add<-round((numfaces*0.08) * 0.48)
female_to_add<-round((numfaces*0.08) * 0.52)
all_categories<-unique(google_clean_agg_binary$Social.Category)

google_clean_agg_aug<-google_clean_agg_binary
aug_male_cat<-sample(all_categories, male_to_add, replace=TRUE)
aug_male_age<-sample(c(1,2,3), male_to_add, replace=TRUE)
aug_male_df<-data.frame(source="Google", Social.Category=aug_male_cat, 
                        face_id = "aug_male_", image_id = "aug_male_", 
                        Img.Gender.Mode = "Male", Img.Age.mode = aug_male_age, 
                        Img.Age.avg = aug_male_age)
aug_male_df$face_id<-paste(aug_male_df$face_id, 1:nrow(aug_male_df), sep="")
aug_male_df$image_id<-paste(aug_male_df$image_id, 1:nrow(aug_male_df), sep="")
google_clean_agg_aug<-rbind(google_clean_agg_aug, aug_male_df)

aug_female_cat<-sample(all_categories, female_to_add, replace=TRUE)
aug_female_age<-sample(c(5,6,7), female_to_add, replace=TRUE)
aug_female_df<-data.frame(source="Google", Social.Category=aug_female_cat, 
                          face_id = "aug_female_", image_id = "aug_female_", 
                          Img.Gender.Mode = "Female", Img.Age.mode = aug_female_age, 
                          Img.Age.avg = aug_female_age)
aug_female_df$face_id<-paste(aug_female_df$face_id, 1:nrow(aug_female_df), sep="")
aug_female_df$image_id<-paste(aug_female_df$image_id, 1:nrow(aug_female_df), sep="")
google_clean_agg_aug<-rbind(google_clean_agg_aug, aug_female_df)

t.test(subset(google_clean_agg_aug, Img.Gender.Mode=="Female")$Img.Age.mode, 
       subset(google_clean_agg_aug, Img.Gender.Mode=="Male")$Img.Age.mode)

google_clean_agg_aug_cat<-google_clean_agg_aug %>% group_by(Social.Category, Img.Gender.Mode) %>%
  dplyr::summarise(Img.Age.mode=mean(Img.Age.mode))

google_clean_agg_aug_cat$Img.Gender.Mode<-as.factor(google_clean_agg_aug_cat$Img.Gender.Mode)
google_clean_agg_aug %>% group_by(Img.Gender.Mode) %>% dplyr::summarise(Img.Age.mode = mean(Img.Age.mode))

ggplot(google_clean_agg_aug, aes(x = Img.Age.mode, fill=Img.Gender.Mode, color=Img.Gender.Mode, group=Img.Gender.Mode, linetype=Img.Gender.Mode)) + 
  theme_bw() + geom_density(lwd = 2, colour = "black", alpha = 0.7, bw=0.7) +
  scale_linetype_manual(values=c("dotted", "solid")) + scale_fill_manual(values=c("goldenrod", "lightblue")) + 
  xlab("Age of Face") + ggtitle("Google") + 
  theme(legend.text=element_text(size=60),
        legend.position=c(0.25,0.7),
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.5, angle=37, vjust=0.7),
        axis.text.y=element_text(size = 60, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits=c(-1,8), breaks=c(1,2,3,4,5,6,7),labels=c("0-11", "12-17","18-24","25-34","35-54","55-74","+75")) 

#false negative simulation (Wiki)
wiki_numfaces<-length(unique(wiki_main_agg$face_id))
wiki_male_to_add<-round((wiki_numfaces*0.08) * 0.48)
wiki_female_to_add<-round((wiki_numfaces*0.08) * 0.52)
all_categories<-unique(wiki_main_agg$Social.Category)

wiki_main_agg_aug<-wiki_main_agg
wiki_aug_male_cat<-sample(all_categories, wiki_male_to_add, replace=TRUE)
wiki_aug_male_age<-sample(c(1,2,3), wiki_male_to_add, replace=TRUE)
wiki_aug_male_df<-data.frame(source="Wikipedia", Social.Category=wiki_aug_male_cat, 
                             face_id = "wiki_aug_male_", image_id = "wiki_aug_male_", 
                             Img.Gender.Mode = "Male", Img.Age.mode = wiki_aug_male_age, 
                             Img.Age.avg = wiki_aug_male_age)
wiki_aug_male_df$face_id<-paste(wiki_aug_male_df$face_id, 1:nrow(wiki_aug_male_df), sep="")
wiki_aug_male_df$image_id<-paste(wiki_aug_male_df$image_id, 1:nrow(wiki_aug_male_df), sep="")
wiki_main_agg_aug<-rbind(wiki_main_agg_aug, wiki_aug_male_df)

wiki_aug_female_cat<-sample(all_categories, wiki_female_to_add, replace=TRUE)
wiki_aug_female_age<-sample(c(5, 6,7), wiki_female_to_add, replace=TRUE)
wiki_aug_female_df<-data.frame(source="Wikipedia", Social.Category=wiki_aug_female_cat, 
                               face_id = "aug_female_", image_id = "aug_female_", 
                               Img.Gender.Mode = "Female", Img.Age.mode = wiki_aug_female_age, 
                               Img.Age.avg = wiki_aug_female_age)
wiki_aug_female_df$face_id<-paste(wiki_aug_female_df$face_id, 1:nrow(wiki_aug_female_df), sep="")
wiki_aug_female_df$image_id<-paste(wiki_aug_female_df$image_id, 1:nrow(wiki_aug_female_df), sep="")
wiki_main_agg_aug<-rbind(wiki_main_agg_aug, wiki_aug_female_df)

t.test(subset(wiki_main_agg_aug, Img.Gender.Mode=="Female")$Img.Age.mode, 
       subset(wiki_main_agg_aug, Img.Gender.Mode=="Male")$Img.Age.mode)

wiki_main_agg_aug_cat<-wiki_main_agg_aug %>% group_by(Social.Category, Img.Gender.Mode) %>%
  dplyr::summarise(Img.Age.mode=mean(Img.Age.mode))

wiki_main_agg_aug_cat$Img.Gender.Mode<-as.factor(wiki_main_agg_aug_cat$Img.Gender.Mode)

ggplot(wiki_main_agg_aug, aes(x = Img.Age.mode, fill=Img.Gender.Mode, color=Img.Gender.Mode, group=Img.Gender.Mode, linetype=Img.Gender.Mode)) + 
  theme_bw() + geom_density(lwd = 2, colour = "black", alpha = 0.7, bw=0.7) +
  scale_linetype_manual(values=c("dotted", "solid")) + scale_fill_manual(values=c("goldenrod", "lightblue")) + 
  xlab("Age of Face") + ggtitle("Wikipedia") + 
  theme(legend.text=element_text(size=60),
        legend.position=c(0.15,0.7),
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.5, angle=37, vjust=0.7),
        axis.text.y=element_text(size = 60, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits=c(-1,8), breaks=c(1,2,3,4,5,6,7),labels=c("0-11", "12-17","18-24","25-34","35-54","55-74","+75")) 

######################################################################################
#####################Adjusting hypothetical bias simulation###########################
#Would results hold if we assume that coders underpredict the age of younger women####
#and overpredict the age of older women, while making errors on male faces at random?#
######################################################################################

#Adjusting hypothetical bias simulation (Google)##
male_dist<-subset(google_clean_agg, Img.Gender.Mode=="Male")
female_dist<-subset(google_clean_agg, Img.Gender.Mode=="Female")
female_dist$rowid<-1:nrow(female_dist)
female_dist_num<-nrow(female_dist)

#ONLY EXAMINING EFFECTS OF ADJUSTING HYPOTHETICAL BIAS WHEREBY CODERS UNDERPREDICT AGE OF YOUNG WOMEN#
female_dist_low<-subset(female_dist, Img.Age.mode<4)
female_dist_low$rowid<-1:nrow(female_dist_low)
female_dist_low_num<-nrow(female_dist_low)

female_dist_high<-subset(female_dist, Img.Age.mode>4)
female_dist_high$rowid<-1:nrow(female_dist_high)
female_dist_high_num<-nrow(female_dist_high)

sims<-1
sim_out_low<-data.frame()
for(i in 1:sims){
  for(mod_perc in seq(0,1,0.02)){
    samp_size<-round(female_dist_low_num * mod_perc)
    samp_rows<-sample(1:female_dist_low_num, samp_size)
    female_dist_low_unmod<-subset(female_dist_low, !rowid %in% samp_rows) 
    female_dist_low_mod<-subset(female_dist_low, rowid %in% samp_rows) 
    female_dist_low_mod$Img.Age.mode<-female_dist_low_mod$Img.Age.mode + 1
    female_dist_low_final<-rbind(female_dist_low_unmod, female_dist_low_mod)
    sim_data_final<-rbind(female_dist_high, female_dist_low_final)
    comparison<-t.test(sim_data_final$Img.Age.mode, male_dist$Img.Age.mode)
    tstat<-comparison$statistic
    pval<-comparison$p.value
    sim_out_low<-rbind(sim_out_low, data.frame(sim=i, perc=mod_perc, tstat=tstat, pval=pval))
  }
}

sim_out_low_agg<-sim_out_low %>% group_by(perc) %>% 
  dplyr::summarise(tstat=mean(tstat), pval=mean(pval)) %>% 
  mutate(signif=pval<0.001)

ggplot(sim_out_low_agg, aes(x = perc, y = tstat, group=1, color=signif)) + theme_bw() + 
  geom_point(size=6) + geom_line(size=1) + 
  scale_color_manual(values=c("red", "black")) + 
  xlab("% Images with Bias Corrected") + ylab("t-statistic") + 
  theme(legend.text=element_text(size=60),
        legend.position="top",
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_text(size = 60, hjust = 0.5),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.5,  vjust=0.7),
        axis.text.y=element_text(size = 60, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  geom_hline(yintercept = 0, linetype="solid", size=1) + 
  scale_y_continuous(limits = c(-175,20))

#EXAMINING EFFECTS OF ADJUSTING HYPOTHETICAL BIAS WHEREBY CODERS UNDERPREDICT AGE OF YOUNG WOMEN AND#
#OVERPREDICT THE AGE OF OLDER WOMEN#

sims<-1
sim_out<-data.frame()
for(i in 1:sims){
  for(mod_perc in seq(0,1,0.02)){
    samp_size<-round(female_dist_num * mod_perc)
    samp_rows<-sample(1:female_dist_num, samp_size)
    female_dist_unmod<-subset(female_dist, !rowid %in% samp_rows) 
    female_dist_mod<-subset(female_dist, rowid %in% samp_rows) 
    female_dist_mod_low<-subset(female_dist_mod, Img.Age.mode<4)
    female_dist_mod_low$Img.Age.mode<-female_dist_mod_low$Img.Age.mode + 1
    female_dist_mod_high<-subset(female_dist_mod, Img.Age.mode>4)
    female_dist_mod_high$Img.Age.mode<-female_dist_mod_high$Img.Age.mode - 1
    female_dist_mod_final<-rbind(female_dist_mod_low, female_dist_mod_high)
    sim_data_final<-rbind(female_dist_unmod, female_dist_mod_final)
    comparison<-t.test(sim_data_final$Img.Age.mode, male_dist$Img.Age.mode)
    tstat<-comparison$statistic
    pval<-comparison$p.value
    sim_out<-rbind(sim_out, data.frame(sim=i, perc=mod_perc, tstat=tstat, pval=pval))
  }
}

sim_out_agg<-sim_out %>% group_by(perc) %>% 
  dplyr::summarise(tstat=mean(tstat), pval=mean(pval)) %>% 
  mutate(signif=pval<0.001)

ggplot(sim_out_agg, aes(x = perc, y = tstat, group=1)) + theme_bw() + 
  geom_point(size=6) + geom_line(size=1) + 
  xlab("% Images with Bias Corrected") + 
  ylab("t-statistic") + 
  theme(legend.text=element_text(size=60),
        legend.position="none",
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_text(size = 60, hjust = 0.5),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.5,  vjust=0.7),
        axis.text.y=element_text(size = 60, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  geom_hline(yintercept = 0, linetype="solid", size=1) + 
  scale_y_continuous(limits = c(-175,20))

#######################################
##Overlap in Image Results across IPs##
#######################################
IP_data_clean_simp<-IP_data_clean %>% dplyr::select(query, IP ,face_id,image_id, WorkerId, age_num, gender)
colnames(IP_data_clean_simp)[1]<-"Social.Category"
colnames(IP_data_clean_simp)[6]<-"Img.Age"
colnames(IP_data_clean_simp)[7]<-"Img.Gender"

IP_data_NY_simp<-IP_data_NY %>% dplyr::select(Social.Category, face_id, image_id, WorkerId, Img.Age, Img.Gender)
IP_data_NY_simp$IP<-"New York"

IP_full<-rbind(IP_data_clean_simp,IP_data_NY_simp)

IPs<-unique(IP_data_clean_simp(IP_full$IP))

IP_overlap_dt<-data.frame()
for(i in 1:length(IPs)){
  IP_i<-IPs[i]
  print(IP_i)
  IP_i_df<-subset(IP_full, IP == IP_i)
  IP_i_df_imgs<-unique(IP_i_df$image_id)
  
  for(j in 1:length(IPs)){
    IP_j<-IPs[j]
    IP_j_df<-subset(IP_full, IP == IP_j)
    IP_j_df_imgs<-unique(IP_j_df$image_id)
    
    img_overlap<-round(length(intersect(IP_i_df_imgs, IP_j_df_imgs))/length(unique(IP_i_df_imgs, IP_j_df_imgs)),2)
    
    IP_overlap_dt<-rbind(IP_overlap_dt, 
                          data.frame(IP_i=IP_i, IP_j=IP_j, img_overlap=img_overlap))
    
  }
}

ggplot(IP_overlap_dt, aes(x = IP_i, y = IP_j, fill = img_overlap)) +
  geom_tile(color = "black") +theme_bw() + 
  geom_text(aes(label = img_overlap), color = "white", size = 8) +
  coord_fixed() + 
  scale_fill_continuous(limits=c(0, 1), breaks=seq(0,1,by=0.2), name="Image\nOverlap") + 
  theme(axis.title = element_blank(), 
        axis.text = element_text(size=20),
        legend.text = element_text(size=18), 
        legend.title = element_text(size=25)) 




