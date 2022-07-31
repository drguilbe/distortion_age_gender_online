#Replication Script for "Women are Depicted as Significantly Younger than Men in over One Million Images from Google, Wikipedia, and IMDb" 
#May 2022
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
library(rms)
library(multiwayvcov)
library(lmtest)

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

#############
#Google Data#
#############
google_raw<-read.csv("C:/Users/dougl/Desktop/comp-syn/algorithmic_bias/Data/RnR/Final_Submit/Google_data.csv")
google_clean<-subset(google_raw, humface == "Yes" & Attention.Check)
google_main<-subset(google_clean, img.gender %in% c("Male","Female") & searchDEMO=="None")

google_clean_agg<- google_clean %>% group_by(source, Social.Category, searchDEMO, face_id, image_id) %>% 
  dplyr::summarise(img.gender.mode=getmode(img.gender), 
                   img.age.mode=getmode(img.age), 
                   img.age.avg=mean(img.age,na.rm=T))

google_main_agg<- google_main %>% group_by(source, Social.Category, face_id, image_id) %>% 
  dplyr::summarise(img.gender.mode=getmode(img.gender), 
                   img.age.mode=getmode(img.age), 
                   img.age.avg=mean(img.age,na.rm=T))

google_main_agg_stats<-google_main_agg %>% group_by(source, img.gender.mode) %>% 
  dplyr::summarise(avg.img.age.mode.num = mean(img.age.mode),
                   med.img.age.mode.num = median(img.age.mode),
                   mode.img.age.mode.num = getmode(img.age.mode))

##############################
#Wikipedia Data (WIT Dataset)#
##############################
wiki_raw<-read.csv("C:/Users/dougl/Desktop/comp-syn/algorithmic_bias/Data/RnR/Final_Submit/Wiki_data.csv")
wiki_clean<-subset(wiki_raw, humface=="Yes" & Attention.Check)
wiki_main<-subset(wiki_clean, img.gender %in% c("Male", "Female"))

wiki_main_agg<- wiki_main %>% group_by(source,Social.Category, face_id, image_id) %>% 
  dplyr::summarise(img.gender.mode=getmode(img.gender), 
                   img.age.mode=getmode(img.age), 
                   img.age.avg=mean(img.age,na.rm=T))

Wiki_Age_Stats<-wiki_main_agg  %>% group_by(source, img.gender.mode) %>% 
  dplyr::summarise(avg.img.age.mode.num = mean(img.age.mode),
                   med.img.age.mode.num = median(img.age.mode),
                   mode.img.age.mode.num = getmode(img.age.mode))

###############################
#IMDb Data (IMDb-Wiki Dataset)#
###############################
IMDBceleb<-read.csv("C:/Users/dougl/Desktop/comp-syn/algorithmic_bias/Data/RnR/Final_Submit/IMDb_celeb_data.csv")
IMDBceleb<-subset(IMDBceleb, gender %in% c(0,1))
IMDBceleb<-subset(IMDBceleb, age > 0 & age <= 100)
IMDBceleb$gender_cat<-as.factor(IMDBceleb$gender)
levels(IMDBceleb$gender_cat)<-c("Female", "Male")

####################################
#Wikipedia Data (IMDb-Wiki Dataset)#
####################################
wikiceleb<-read.csv("C:/Users/dougl/Desktop/comp-syn/algorithmic_bias/Data/RnR/Final_Submit/Wiki_celeb_data.csv")
wikiceleb<-subset(wikiceleb, gender %in% c(0,1))
wikiceleb<-subset(wikiceleb, age > 0 & age <= 100)
wikiceleb$gender_cat<-as.factor(wikiceleb$gender)
levels(wikiceleb$gender_cat)<-c("Female", "Male")

############################
#Google Data (CACD Dataset)#
############################
CADC<-read.csv("C:/Users/dougl/Desktop/comp-syn/algorithmic_bias/Data/RnR/Final_Submit/CADC_data.csv")
IMDBceleb_simp<-unique(IMDBceleb %>% select(name, gender))
CADC_comp<-merge(IMDBceleb_simp, CADC, by=c("name"))
CADC_comp$gender<-as.factor(CADC_comp$gender)
levels(CADC_comp$gender)<-c("Female", "Male")

##############
#Main Results#
##############

#########
#Fig. 2A#
#########
ggplot(google_main_agg, aes(x = img.age.mode, fill=img.gender.mode, color=img.gender.mode, group=img.gender.mode, linetype=img.gender.mode)) + 
  theme_bw() + geom_density(lwd = 2, colour = "black", alpha = 0.7, bw = 0.7) +
  scale_linetype_manual(values=c("dotted", "solid")) + scale_fill_manual(values=c("goldenrod", "lightblue")) + 
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
  scale_x_continuous(limits=c(-1,8), breaks=c(1,2,3,4,5,6,7),labels=c("0-11", "12-17","18-24","25-34","35-54","55-74","+75")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + coord_cartesian(ylim=c(0,0.35)) + 
  geom_vline(xintercept = subset(google_main_agg_stats, img.gender.mode=="Female")$avg.img.age.mode.num, 
             color="goldenrod", size=4, alpha=0.8, linetype="dashed") + 
  geom_vline(xintercept = subset(google_main_agg_stats, img.gender.mode=="Male")$avg.img.age.mode.num, 
             color="blue", size=4, alpha=0.8) 

#Fig 2A Stats#
t.test(subset(google_main_agg, img.gender.mode=="Female")$img.age.mode, 
       subset(google_main_agg, img.gender.mode=="Male")$img.age.mode)

#Fig 2A Stats (Matched by Category)#
google_main_cat_match<- google_main_agg %>% group_by(Social.Category) %>%
  dplyr::summarise(fem.age.avg=mean(img.age.mode[img.gender.mode=="Female"],na.rm=T),
                   fem.freq = length(img.age.mode[img.gender.mode=="Female"]), 
                   mal.age.avg=mean(img.age.mode[img.gender.mode=="Male"],na.rm=T), 
                   mal.freq = length(img.age.mode[img.gender.mode=="Male"]))

google_main_cat_match<-google_main_cat_match[complete.cases(google_main_cat_match),]
t.test(google_main_cat_match$fem.age.avg, google_main_cat_match$mal.age.avg,paired=T)

#########
#Fig. 2B#
#########
ggplot(wiki_main_agg, aes(x = img.age.mode, fill=img.gender.mode, color=img.gender.mode, 
                          group=img.gender.mode, linetype=img.gender.mode)) + theme_bw() + 
  geom_density(lwd = 2, colour = "black", alpha = 0.7, bw = 0.7) +
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
  coord_cartesian(ylim=c(0,0.35)) + 
  geom_vline(xintercept = subset(Wiki_Age_Stats, img.gender.mode=="Female")$avg.img.age.mode.num, 
             color="goldenrod", size=4, alpha=0.8, linetype="dashed") + 
  geom_vline(xintercept = subset(Wiki_Age_Stats, img.gender.mode=="Male")$avg.img.age.mode.num, 
             color="blue", size=4, alpha=0.8)

#Fig 2B Stats#
t.test(subset(wiki_main_agg, img.gender.mode=="Female")$img.age.mode, 
       subset(wiki_main_agg, img.gender.mode=="Male")$img.age.mode)

#Fig 2B Stats (Matched by Category)#
wiki_main_cat_match<-wiki_main_agg %>% group_by(source, Social.Category) %>%
  dplyr::summarise(fem.age.avg=mean(img.age.mode[img.gender.mode=="Female"],na.rm=T),
                   fem.freq = length(img.age.mode[img.gender.mode=="Female"]), 
                   mal.age.avg=mean(img.age.mode[img.gender.mode=="Male"],na.rm=T),
                   mal.freq = length(img.age.mode[img.gender.mode=="Male"]))

wiki_main_cat_match<-wiki_main_cat_match[complete.cases(wiki_main_cat_match),]
t.test(wiki_main_cat_match$fem.age.avg, wiki_main_cat_match$mal.age.avg,paired=T)

#########
#Fig. 2C#
#########
google_gendered<-subset(google_clean_agg, searchDEMO != "None" & img.gender.mode %in% c("Male","Female"))

google_gendered_agg<-google_gendered %>% group_by(source, Social.Category, img.gender.mode) %>% 
  dplyr::summarise(img.age.mode = mean(img.age.mode, na.rm=T), 
                   num_faces = length(unique(face_id)))

google_gendered_agg_cat_match<-google_gendered_agg %>% group_by(source, Social.Category) %>%
  dplyr::summarise(fem.age.avg=mean(img.age.mode[img.gender.mode=="Female"],na.rm=T),
                   fem.freq = length(img.age.mode[img.gender.mode=="Female"]), 
                   mal.age.avg=mean(img.age.mode[img.gender.mode=="Male"],na.rm=T),
                   mal.freq = length(img.age.mode[img.gender.mode=="Male"]))

google_gendered_agg_cat_match<-google_gendered_agg_cat_match[complete.cases(google_gendered_agg_cat_match),]

google_gendered_match_fem<-google_gendered_agg_cat_match %>% select(source, Social.Category, fem.age.avg)
colnames(google_gendered_match_fem)[3]<-"img.age.avg"
google_gendered_match_fem$img.gender.mode<-"Female"
google_gendered_match_male<-google_gendered_agg_cat_match %>% select(source, Social.Category, mal.age.avg)
colnames(google_gendered_match_male)[3]<-"img.age.avg"
google_gendered_match_male$img.gender.mode<-"Male"
google_gendered_match<-rbind(google_gendered_match_fem, google_gendered_match_male)

google_gendered_match_stat<-google_gendered_match %>% group_by(source, img.gender.mode) %>% 
  dplyr::summarise(img.age.avg=mean(img.age.avg))

ggplot(google_gendered_match, 
       aes(x = img.age.avg, fill=img.gender.mode, color=img.gender.mode, group=img.gender.mode, linetype=img.gender.mode)) + theme_bw() + 
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
  geom_vline(xintercept = subset(google_gendered_match_stat, img.gender.mode=="Female")$img.age.avg, 
             color="goldenrod", size=4, alpha=0.8, linetype="dashed") + 
  geom_vline(xintercept = subset(google_gendered_match_stat, img.gender.mode=="Male")$img.age.avg, 
             color="blue", size=4, alpha=0.8) 

#Fig. 2C Stats#
t.test(google_gendered_agg_cat_match$fem.age.avg, google_gendered_agg_cat_match$mal.age.avg,paired=T)

#########
#Fig. 2D#
#########
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

#Fig. 2D Stats#
imdb_female=subset(IMDBceleb, gender==0)
imdb_male=subset(IMDBceleb, gender==1)
t.test(imdb_female$age, imdb_male$age)

#########
#Fig. 2E#
#########
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

#Fig. 2E Stats#
wiki_female=subset(wikiceleb, gender==0)
wiki_male=subset(wikiceleb, gender==1)
t.test(wiki_female$age, wiki_male$age)

#########
#Fig. 2F#
#########
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

#Fig. 2F Stats#
t.test(subset(CADC_comp, gender=="Male")$age,subset(CADC_comp, gender=="Female")$age)

########################
#Supplementary Analyses#
########################

#######################################
#Controlling for inter-coder agreement#
#######################################

#Google#
google_main_max_freq_gender<-google_main %>% 
  group_by(face_id,img.gender, Social.Category) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>% 
  group_by(face_id, Social.Category) %>% 
  dplyr::summarise(max_agree_gender=max(freq))

google_main_max_freq_age<-google_main %>% 
  group_by(face_id,img.age, Social.Category) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>% 
  group_by(face_id, Social.Category) %>% 
  dplyr::summarise(max_agree_age=max(freq))

google_main_agg_m<-merge(google_main_agg, google_main_max_freq_gender, by=c("face_id", "Social.Category"))
google_main_agg_m<-merge(google_main_agg_m, google_main_max_freq_age, by=c("face_id", "Social.Category"))

google_main_agg_m$img.gender.mode.binary<-as.factor(google_main_agg_m$img.gender.mode)
levels(google_main_agg_m$img.gender.mode.binary)<-c(0,1)
google_main_agg_m_comp<-google_main_agg_m[complete.cases(google_main_agg_m),]

#Model: Predicting Age, controlling for intercoder agreement on gender#
google_cntrl_coder_agree<-lm(img.age.mode ~ img.gender.mode + max_agree_gender, data=google_main_agg_m)
summary(google_cntrl_coder_agree)
#Cluster Standard Errors
google_cntrl_coder_agree_vcov <- cluster.vcov(google_cntrl_coder_agree, google_main_agg_m$Social.Category)
coeftest(google_cntrl_coder_agree, google_cntrl_coder_agree_vcov)

#Model: Predicting Gender, controlling for intercoder agreement on age#
google_mod_log=lrm(img.gender.mode.binary ~ img.age.mode + max_agree_age, x=T, y=T, data=google_main_agg_m_comp)
robcov(google_mod_log, cluster=google_main_agg_m_comp$Social.Category)

unit.change=c(1)
exp(coef(google_mod_log)["img.age.mode"]*unit.change)

#Wikipedia
wiki_main_max_freq_gender<-wiki_main %>% 
  group_by(face_id, img.gender, Social.Category) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>% 
  group_by(face_id, Social.Category) %>% 
  dplyr::summarise(max_agree_gender=max(freq))

wiki_main_max_freq_age<-wiki_main %>% 
  group_by(face_id, img.age, Social.Category) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>% 
  group_by(face_id, Social.Category) %>% 
  dplyr::summarise(max_agree_age=max(freq))

wiki_main_agg_m<-merge(wiki_main_agg, wiki_main_max_freq_gender, by=c("face_id", "Social.Category"))
wiki_main_agg_m<-merge(wiki_main_agg_m, wiki_main_max_freq_age, by=c("face_id", "Social.Category"))

#Model: Predicting Age, controlling for intercoder agreement on gender#
wiki_mod<-lm(img.age.mode ~ img.gender.mode + max_agree_gender, data=wiki_main_agg_m)
summary(wiki_mod)
wiki_mod_vcov <- cluster.vcov(wiki_mod, wiki_main_agg_m$Social.Category)
coeftest(wiki_mod, wiki_mod_vcov)

wiki_main_agg_m$img.gender.mode.binary<-as.factor(wiki_main_agg_m$img.gender.mode)
levels(wiki_main_agg_m$img.gender.mode.binary)<-c(0,1)
wiki_main_agg_m_comp<-wiki_main_agg_m[complete.cases(wiki_main_agg_m),]

#Model: Predicting Gender, controlling for intercoder agreement on age#
wiki_mod_log=lrm(img.gender.mode.binary ~ img.age.mode + max_agree_age, x=T, y=T, data=wiki_main_agg_m_comp)
robcov(wiki_mod_log, cluster=wiki_main_agg_m_comp$Social.Category)
unit.change=c(1)
exp(coef(wiki_mod_log)["img.age.mode"]*unit.change)
unit.change.agree=c(0.1)
exp(coef(wiki_mod_log)["max_agree_age"]*unit.change)

######################################
##Controlling for Coder Demographics##
######################################

#############################
#Google Intercoder Agreement#
#############################
google_demo_simp<-unique(google_clean %>% select(source, WorkerId, mt_poli.orient, mt_gender, mt_race, mt_age, mt_age_cat, mt_income, mt_education))
wiki_demo_simp<-unique(wiki_clean %>% select(source, WorkerId, mt_poli.orient, mt_gender, mt_race, mt_age, mt_age_cat, mt_income, mt_education))
demo_all<-rbind(google_demo_simp, wiki_demo_simp)
length(unique(demo_all$WorkerId))

table(demo_all$mt_gender)/sum(table(demo_all$mt_gender))
table(demo_all$mt_race)/sum(table(demo_all$mt_race))
table(demo_all$mt_age)/sum(table(demo_all$mt_age))
table(demo_all$mt_poli.orient)/sum(table(demo_all$mt_poli.orient))
table(demo_all$mt_income)/sum(table(demo_all$mt_income))

fem_demo_all<-subset(demo_all, mt_gender=="Female")
table(fem_demo_all$mt_age)/sum(table(fem_demo_all$mt_age))
male_demo_all<-subset(demo_all, mt_gender=="Male")
table(demo_all$mt_age)/sum(table(demo_all$mt_age))

#Model Predicting Outcome Controlling for Demographics
google_clean_simp<-subset(google_clean, !img.gender %in% c("Non-binary"))

Google_mod_demo<-lm(img.age ~ img.gender + mt_gender + mt_age, data = google_clean_simp)
summary(Google_mod_demo)
Google_mod_demo_vcov <- cluster.vcov(Google_mod_demo, google_clean_simp$Social.Category)
coeftest(Google_mod_demo, Google_mod_demo_vcov)

#Wikipedia demographics 
wiki_clean_simp<-subset(wiki_clean, !img.gender %in% c("Non-binary"))

wiki_mod_demo<-lm(img.age ~ img.gender + mt_gender + mt_age, data = wiki_clean_simp)
summary(wiki_mod_demo)

wiki_mod_demo_vcov <- cluster.vcov(wiki_mod_demo, wiki_clean_simp$Social.Category)
coeftest(wiki_mod_demo, wiki_mod_demo_vcov)

#####Controlling for Number of Faces in Each Image################

#Google
google_clean_wNumFaces<-google_clean_simp %>% group_by(image_id) %>% 
  dplyr::summarise(num_faces = length(unique(face_id)))

google_clean_wNumFaces_full<-merge(google_clean_simp, google_clean_wNumFaces, by=c("image_id"), all=T)

Google_mod_demo_wNumFace<-lm(img.age ~ img.gender + num_faces, data = google_clean_wNumFaces_full)
summary(Google_mod_demo_wNumFace)

Google_mod_demo_wNumFace_vcov <- cluster.vcov(Google_mod_demo_wNumFace, google_clean_wNumFaces_full$Social.Category)
coeftest(Google_mod_demo_wNumFace, Google_mod_demo_wNumFace_vcov)

#Wikipedia
wiki_clean_wNumFaces<-wiki_clean_simp %>% group_by(image_id) %>% 
  dplyr::summarise(num_faces = length(unique(face_id)))

wiki_clean_wNumFaces_full<-merge(wiki_clean_simp, wiki_clean_wNumFaces, by=c("image_id"), all=T)

wiki_mod_demo_wNumFace<-lm(img.age ~ img.gender + num_faces, data = wiki_clean_wNumFaces_full)
summary(wiki_mod_demo_wNumFace)

wiki_mod_demo_wNumFace_vcov <- cluster.vcov(wiki_mod_demo_wNumFace, wiki_clean_wNumFaces_full$Social.Category)
coeftest(wiki_mod_demo_wNumFace, wiki_mod_demo_wNumFace_vcov)

#Controlling for frequency in Google Trends 
google_trends_freq<-read.csv("C:/Users/dougl/Desktop/comp-syn/algorithmic_bias/Data/RnR/Final_Submit/Google_Search_Frequency_data.csv")
google_main_wfreq<-merge(google_main, google_trends_freq, by=c("Social.Category"))

google_freq_mod<-lm(img.age ~ img.gender + Google.Img.US.Search.Freq, data=google_main_wfreq)
summary(google_freq_mod)

google_freq_vcov_category <- cluster.vcov(google_freq_mod, google_main_wfreq$Social.Category)
coeftest(google_freq_mod, google_freq_vcov_category)

google_freq_vcov_coder <- cluster.vcov(google_freq_mod, google_main_wfreq$WorkerId)
coeftest(google_freq_mod, google_freq_vcov_coder)

#Robustness aggregation method
t.test(subset(google_main_agg, img.gender.mode == "Female")$img.age.avg, 
       subset(google_main_agg, img.gender.mode == "Male")$img.age.avg)

google_clean_agg_gendered<-subset(google_clean_agg, searchDEMO %in% c("Female","Male"))
t.test(subset(google_clean_agg_gendered, img.gender.mode == "Female")$img.age.avg, 
       subset(google_clean_agg_gendered, img.gender.mode == "Male")$img.age.avg)

t.test(subset(wiki_main_agg, img.gender.mode == "Female")$img.age.avg, 
       subset(wiki_main_agg, img.gender.mode == "Male")$img.age.avg)

#Robustness to image cropping
uncropped<-read.csv("C:/Users/dougl/Desktop/comp-syn/algorithmic_bias/Data/RnR/Final_Submit/Google_data_uncropped_sample.csv")
uncropped_clean<-subset(uncropped, humface=="Yes" & Attention.Check & img.gender %in% c("Male", "Female"))

uncropped_stats<-uncropped_clean %>% group_by(img.gender) %>% 
  dplyr::summarise(avg_age=mean(img.age), 
                   modal_age=getmode(img.age))

ggplot(uncropped_clean, aes(x = img.age, fill=img.gender, color=img.gender, group=img.gender, linetype=img.gender)) + 
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
  geom_vline(xintercept = subset(uncropped_stats, img.gender=="Female")$modal_age, 
             color="goldenrod", size=4, alpha=0.8, linetype="dashed") + 
  geom_vline(xintercept = subset(uncropped_stats, img.gender=="Male")$modal_age, 
             color="blue", size=4, alpha=0.8)

t.test(subset(uncropped_clean, img.gender=="Female")$img.age, subset(uncropped_clean, img.gender=="Male")$img.age)
