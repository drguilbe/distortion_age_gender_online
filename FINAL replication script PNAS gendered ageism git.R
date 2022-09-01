#Replication Script for "Women are Depicted as Significantly Younger than Men in over One Million Images from Google, Wikipedia, and IMDb" 
#October 2022
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

google_clean_agg<- google_clean %>% group_by(source, Social.Category, searchDEMO, face_id, image_id, Gendered.Category) %>% 
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
google_gendered<-subset(google_clean_agg, searchDEMO != "None" & img.gender.mode %in% c("Male","Female") & Gendered.Category == 0)

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
  geom_vline(xintercept = mean(subset(wikiceleb, gender_cat=="Female")$age, na.rm=T), 
             color="goldenrod", size=4, alpha=0.8, linetype="dashed") + 
  geom_vline(xintercept = mean(subset(wikiceleb, gender_cat=="Male")$age, na.rm=T), color="blue", size=4, alpha=0.8)

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

##############################
##MAIN RESULTS WITH RAW DATA##
##############################
google_raw_agg<- google_raw %>% group_by(source, Social.Category, searchDEMO, face_id, image_id, Gendered.Category) %>% 
  dplyr::summarise(img.gender.mode=getmode(img.gender), 
                   img.age.mode=getmode(img.age), 
                   img.age.avg=mean(img.age,na.rm=T))

t.test(subset(google_raw_agg, img.gender.mode=="Female")$img.age.mode, subset(google_raw_agg, img.gender.mode=="Male")$img.age.mode)
t.test(subset(google_raw, img.gender=="Female")$img.age, subset(google_raw, img.gender=="Male")$img.age)

######################################
##Controlling for Coder Demographics##
######################################
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

############################
#Controlling for coders FEs#
############################
wiki_cntrl_coderFEs<-lm(img.age ~ img.gender + WorkerId, data=wiki_main)
summary(wiki_cntrl_coderFEs)
#Cluster Standard Errors
wiki_cntrl_coderFEs_vcov <- cluster.vcov(wiki_cntrl_coderFEs, wiki_main$Social.Category)
coeftest(wiki_cntrl_coderFEs, wiki_cntrl_coderFEs_vcov)

google_cntrl_coderFEs<-lm(img.age ~ img.gender + WorkerId, data=google_main)
summary(google_cntrl_coderFEs)
#Cluster Standard Errors
google_cntrl_coderFEs_vcov <- cluster.vcov(google_cntrl_coderFEs, google_main$Social.Category)
coeftest(google_cntrl_coderFEs, google_cntrl_coderFEs_vcov)

#######################################
#Controlling for inter-coder agreement#
#######################################

#Google Intercoder agreement#
google_main_max_freq_gender<-google_main %>% 
  group_by(face_id, img.gender, Social.Category) %>% 
  dplyr::summarise(n_uni_worker=length(unique(WorkerId))) %>%
  group_by(face_id, Social.Category) %>% 
  mutate(nworkers=sum(n_uni_worker)) %>% 
  group_by(face_id, img.gender, Social.Category) %>% 
  mutate(propAgree=n_uni_worker/nworkers) %>% 
  group_by(face_id, Social.Category) %>% 
  dplyr::summarise(max_agree_gender=max(propAgree), n=sum(nworkers))

google_main_max_freq_age<-google_main %>% 
  group_by(face_id, img.age, Social.Category) %>% 
  dplyr::summarise(n_uni_worker=length(unique(WorkerId))) %>%
  group_by(face_id, Social.Category) %>% 
  mutate(nworkers=sum(n_uni_worker)) %>% 
  group_by(face_id, img.age, Social.Category) %>% 
  mutate(propAgree=n_uni_worker/nworkers) %>% 
  group_by(face_id, Social.Category) %>% 
  dplyr::summarise(max_agree_age=max(propAgree), n=sum(nworkers))

google_main_agg_m<-merge(google_main_agg, google_main_max_freq_gender, by=c("face_id", "Social.Category"))
google_main_agg_m<-merge(google_main_agg_m, google_main_max_freq_age, by=c("face_id", "Social.Category"))

google_main_agg_m$img.gender.mode.binary<-as.factor(google_main_agg_m$img.gender.mode)
levels(google_main_agg_m$img.gender.mode.binary)<-c(0,1)
google_main_agg_m_comp<-google_main_agg_m[complete.cases(google_main_agg_m),]

#Google Model: Predicting Age, controlling for intercoder agreement on gender#
google_cntrl_coder_agree<-lm(img.age.mode ~ img.gender.mode + max_agree_gender, data=google_main_agg_m)
summary(google_cntrl_coder_agree)
google_cntrl_coder_agree_vcov <- cluster.vcov(google_cntrl_coder_agree, google_main_agg_m$Social.Category)
coeftest(google_cntrl_coder_agree, google_cntrl_coder_agree_vcov)

#Google Model: Predicting Gender, controlling for intercoder agreement on age#
google_mod_log=lrm(img.gender.mode.binary ~ img.age.mode + max_agree_age, x=T, y=T, data=google_main_agg_m_comp)
robcov(google_mod_log, cluster=google_main_agg_m_comp$Social.Category)
unit.change=c(1)
exp(coef(google_mod_log)["img.age.mode"]*unit.change)

#Model gender agreement with all Google data (clean)
google_clean_max_freq_gender<-google_clean %>% 
  group_by(face_id, img.gender, Social.Category) %>% 
  dplyr::summarise(n_uni_worker=length(unique(WorkerId))) %>%
  group_by(face_id, Social.Category) %>% 
  mutate(nworkers=sum(n_uni_worker)) %>% 
  group_by(face_id, img.gender, Social.Category) %>% 
  mutate(propAgree=n_uni_worker/nworkers) %>% 
  group_by(face_id, Social.Category) %>% 
  dplyr::summarise(max_agree_gender=max(propAgree), n=sum(nworkers))

#Examining only post-process images with exactly 3 coders (Google)
google_clean_max_freq_gender_c3<-subset(google_clean_max_freq_gender, n==3)
#Examine intercoder agreement among images with more than 3 coders
google_clean_max_freq_gender_cn<-subset(google_clean_max_freq_gender, n>3)

#Model age agreement with all Google data (clean)
google_clean_max_freq_age<-google_clean %>% 
  group_by(face_id, img_age_cat, Social.Category) %>% 
  dplyr::summarise(n_uni_worker=length(unique(WorkerId))) %>%
  group_by(face_id, Social.Category) %>% 
  mutate(nworkers=sum(n_uni_worker)) %>% 
  group_by(face_id, img_age_cat, Social.Category) %>% 
  mutate(propAgree=n_uni_worker/nworkers) %>% 
  group_by(face_id, Social.Category) %>% 
  dplyr::summarise(max_agree_age=max(propAgree), n=sum(nworkers))

#Examining only post-process images with exactly 3 coders (Google)
google_clean_max_freq_age_c3<-subset(google_clean_max_freq_age, n==3)
#Examine intercoder agreement among images with more than 3 coders (Google)
google_clean_max_freq_age_cn<-subset(google_clean_max_freq_age, n>3)

#distance of age classifications in general (Google)#
google_clean_diff<-google_clean %>% 
  group_by(face_id) %>% dplyr::summarise(dist=mean(diff(img.age, na.rm=T), na.rm=T))
google_clean_diff[google_clean_diff$dist=="NaN",]$dist<-0
mean(google_clean_diff$dist)
sum(google_clean_diff$dist<1)/nrow(google_clean_diff)
sum(google_clean_diff$dist<2)/nrow(google_clean_diff)

#Robustness to analyzing only images with exactly 3 coders (Google)
gender_max_robust<-subset(google_clean_max_freq_gender_c3, max_agree_gender==1)
age_max_robust<-subset(google_clean_max_freq_age_c3, max_agree_age==1)
max_robust_faces<-unique(c(gender_max_robust$face_id, age_max_robust$face_id))
google_clean_agg_max_robust<-subset(google_clean_agg, face_id %in% max_robust_faces)
google_clean_agg_max_robust<-subset(google_clean_agg_max_robust,img.gender.mode %in% c("Male","Female"))

ggplot(google_clean_agg_max_robust, aes(x = img.age.mode, fill=img.gender.mode, color=img.gender.mode, group=img.gender.mode, linetype=img.gender.mode)) + 
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
  scale_x_continuous(limits=c(-1,8), breaks=c(1,2,3,4,5,6,7),labels=c("0-11", "12-17","18-24","25-34","35-54","55-74","+75")) + 
  geom_vline(xintercept = mean(subset(google_clean_agg_max_robust, img.gender.mode=="Male")$img.age.mode), 
             color="blue", size=2, alpha=1, linetype="solid") + 
  geom_vline(xintercept = mean(subset(google_clean_agg_max_robust, img.gender.mode=="Female")$img.age.mode),
             color="goldenrod", size=2, alpha=1) 

t.test(subset(google_clean_agg_max_robust, img.gender.mode=="Female")$img.age.mode, 
       subset(google_clean_agg_max_robust, img.gender.mode=="Male")$img.age.mode)

###WIKIPEDIA intercoder agreement###

#Model gender agreement with all Wiki data
wiki_main_max_freq_gender<-wiki_main %>% 
  group_by(face_id, img.gender, Social.Category) %>% 
  dplyr::summarise(n_uni_worker=length(unique(WorkerId))) %>%
  group_by(face_id, Social.Category) %>% 
  mutate(nworkers=sum(n_uni_worker)) %>% 
  group_by(face_id, img.gender, Social.Category) %>% 
  mutate(propAgree=n_uni_worker/nworkers) %>% 
  group_by(face_id, Social.Category) %>% 
  dplyr::summarise(max_agree_gender=max(propAgree), n=sum(nworkers))

#Examining only post-process images with exactly 3 coders (Wiki)
wiki_main_max_freq_gender_c3<-subset(wiki_main_max_freq_gender, n==3)
table(wiki_main_max_freq_gender_c3$max_agree_gender)/sum(table(wiki_main_max_freq_gender_c3$max_agree_gender))
#Examine intercoder agreement among images with more than 3 coders (Wiki)
wiki_main_max_freq_gender_cn<-subset(wiki_main_max_freq_gender, n>3)
mean(wiki_main_max_freq_gender_cn$n)
sd(wiki_main_max_freq_gender_cn$n)
mean(wiki_main_max_freq_gender_cn$max_agree_gender)
table(wiki_main_max_freq_gender_cn$max_agree_gender)/sum(table(wiki_main_max_freq_gender_cn$max_agree_gender))

#merge with modal classification
wiki_main_agg_cn<-merge(wiki_main_agg, wiki_main_max_freq_gender_cn, by=c("face_id", "Social.Category"))
t.test(subset(wiki_main_agg_cn, img.gender.mode=="Female")$max_agree_gender, 
       subset(wiki_main_agg_cn, img.gender.mode=="Male")$max_agree_gender)
table(wiki_main_agg_cn$img.gender.mode)/sum(table(wiki_main_agg_cn$img.gender.mode))
table(wiki_main_agg_cn$img.age.mode)/sum(table(wiki_main_agg_cn$img.age.mode))

#Model age agreement with all Wiki data
wiki_main_max_freq_age<-wiki_main %>% 
  group_by(face_id, img.age.cat, Social.Category) %>% 
  dplyr::summarise(n_uni_worker=length(unique(WorkerId))) %>%
  group_by(face_id, Social.Category) %>% 
  mutate(nworkers=sum(n_uni_worker)) %>% 
  group_by(face_id, img.age.cat, Social.Category) %>% 
  mutate(propAgree=n_uni_worker/nworkers) %>% 
  group_by(face_id, Social.Category) %>% 
  dplyr::summarise(max_agree_age=max(propAgree), n=sum(nworkers))

table(wiki_main_max_freq_age$max_agree_age)/sum(table(wiki_main_max_freq_age$max_agree_age))
#Examining only post-process images with exactly 3 coders (Wiki)
wiki_main_max_freq_age_c3<-subset(wiki_main_max_freq_age, n==3)
table(wiki_main_max_freq_age_c3$max_agree_age)/sum(table(wiki_main_max_freq_age_c3$max_agree_age))
#Examine intercoder agreement among images with more than 3 coders (Wiki)
wiki_main_max_freq_age_cn<-subset(wiki_main_max_freq_age, n>3)
mean(wiki_main_max_freq_age_cn$n)
sd(wiki_main_max_freq_age_cn$n)
mean(wiki_main_max_freq_age_cn$max_agree_age)

wiki_main_age_cn<-subset(wiki_main, face_id %in% wiki_main_max_freq_age_cn$face_id)
wiki_main_agg_age_cn<-subset(wiki_main_agg, face_id %in% wiki_main_max_freq_age_cn$face_id)
wiki_main_agg_age_cn$freq<-1
wiki_main_agg_age_cn_agg<-wiki_main_agg_age_cn %>% group_by(face_id, img.age.mode) %>%
  dplyr::summarise(sumfreq=sum(freq), sumfreq_cat=sumfreq==1)

table(wiki_main_agg_age_cn_agg$sumfreq_cat)/sum(table(wiki_main_agg_age_cn_agg$sumfreq_cat))
wiki_main_agg_age_cn_agg_diff<-subset(wiki_main_agg_age_cn_agg, !sumfreq_cat)
table(wiki_main_agg_age_cn_agg_diff$img.age.mode)/sum(table(wiki_main_agg_age_cn_agg_diff$img.age.mode))

#distance of age classifications in general (Wiki)#
wiki_main_diff<-wiki_main %>% 
  group_by(face_id) %>% dplyr::summarise(dist=mean(diff(img.age, na.rm=T), na.rm=T))
wiki_main_diff[wiki_main_diff$dist=="NaN",]$dist<-0
mean(wiki_main_diff$dist)
sum(wiki_main_diff$dist<1)/nrow(wiki_main_diff)
sum(wiki_main_diff$dist<2)/nrow(wiki_main_diff)

#distance of age classifications for images classified by more than 3 coders (Wiki)#
wiki_main_age_cn_diff<-wiki_main_age_cn %>% 
  group_by(face_id) %>% dplyr::summarise(dist=mean(diff(img.age), na.rm=T))
wiki_main_age_cn_diff_disagg<-subset(wiki_main_age_cn_diff, dist>0)
mean(wiki_main_age_cn_diff_disagg$dist)
sum(wiki_main_age_cn_diff_disagg$dist<=1)/nrow(wiki_main_age_cn_diff_disagg)

#Robustness to analyzing only images with exactly 3 coders (Wiki)#
wiki_gender_max_robust<-subset(wiki_main_max_freq_gender_c3, max_agree_gender==1)
wiki_age_max_robust<-subset(wiki_main_max_freq_age_c3, max_agree_age==1)
wiki_max_robust_faces<-unique(c(wiki_gender_max_robust$face_id, wiki_age_max_robust$face_id))
wiki_main_agg_max_robust<-subset(wiki_main_agg, face_id %in% wiki_max_robust_faces)
wiki_main_agg_max_robust<-subset(wiki_main_agg_max_robust,img.gender.mode %in% c("Male","Female"))

ggplot(wiki_main_agg_max_robust, aes(x = img.age.mode, fill=img.gender.mode, color=img.gender.mode, group=img.gender.mode, linetype=img.gender.mode)) + 
  theme_bw() + geom_density(lwd = 2, colour = "black", alpha = 0.7, bw=0.5) +
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
  geom_vline(xintercept = mean(subset(wiki_main_agg_max_robust, img.gender.mode=="Male")$img.age.mode), 
             color="blue", size=2, alpha=1, linetype="solid") + 
  geom_vline(xintercept = mean(subset(wiki_main_agg_max_robust, img.gender.mode=="Female")$img.age.mode),
             color="goldenrod", size=2, alpha=1) 

t.test(subset(wiki_main_agg_max_robust, img.gender.mode=="Female")$img.age.mode, 
       subset(wiki_main_agg_max_robust, img.gender.mode=="Male")$img.age.mode)

######################################################
#Controlling for images that repeated across searches#
######################################################

#repeat images (Google)#
google_clean$demo_cat<-paste(google_clean$searchDEMO, google_clean$Social.Category, sep="_")
google_repeat_check<-google_clean %>% group_by(face_id) %>% dplyr::summarise(numCats=length(unique(demo_cat)))
table(google_repeat_check$numCats)/sum(table(google_repeat_check$numCats))
google_repeat_check_norep<-subset(google_repeat_check, numCats==1)
google_clean_norep<-subset(google_clean, face_id %in% google_repeat_check_norep$face_id)
google_clean_norep_main<-subset(google_clean_norep, !img.gender %in% c("Non-binary"))

google_clean_norep_main_agg<- google_clean_norep_main %>% group_by(source, Social.Category, searchDEMO, face_id, image_id, Gendered.Category) %>% 
  dplyr::summarise(img.gender.mode=getmode(img.gender), 
                   img.age.mode=getmode(img.age), 
                   img.age.avg=mean(img.age,na.rm=T))

ggplot(google_clean_norep_main_agg, aes(x = img.age.mode, fill=img.gender.mode, color=img.gender.mode, group=img.gender.mode, linetype=img.gender.mode)) + 
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
  scale_x_continuous(limits=c(-1,8), breaks=c(1,2,3,4,5,6,7),labels=c("0-11", "12-17","18-24","25-34","35-54","55-74","+75")) + 
  geom_vline(xintercept = mean(subset(google_clean_norep_main_agg, img.gender.mode=="Male")$img.age.mode), 
             color="blue", size=2, alpha=1, linetype="solid") + 
  geom_vline(xintercept = mean(subset(google_clean_norep_main_agg, img.gender.mode=="Female")$img.age.mode),
             color="goldenrod", size=2, alpha=1) 

t.test(subset(google_clean_norep_main_agg, img.gender.mode=="Female")$img.age.mode, 
       subset(google_clean_norep_main_agg, img.gender.mode=="Male")$img.age.mode)

google_clean_rep<-subset(google_clean, !face_id %in% google_repeat_check_norep$face_id)
google_clean_rep_main_agg<- google_clean_rep %>% group_by(source, Social.Category, searchDEMO, face_id, image_id, Gendered.Category) %>% 
  dplyr::summarise(img.gender.mode=getmode(img.gender), 
                   img.age.mode=getmode(img.age), 
                   img.age.avg=mean(img.age,na.rm=T))

t.test(subset(google_clean_rep_main_agg, img.gender.mode=="Female")$img.age.mode, 
       subset(google_clean_rep_main_agg, img.gender.mode=="Male")$img.age.mode)

#repeat images (Wiki)#
wiki_repeat_check<-wiki_main %>% group_by(face_id) %>% dplyr::summarise(numCats=length(unique(Social.Category)))
table(wiki_repeat_check$numCats)/sum(table(wiki_repeat_check$numCats))
wiki_repeat_check_norep<-subset(wiki_repeat_check, numCats==1)
wiki_clean_norep<-subset(wiki_main, face_id %in% wiki_repeat_check_norep$face_id)
wiki_clean_norep_main<-subset(wiki_clean_norep, !img.gender %in% c("Non-binary"))

wiki_clean_norep_main_agg<- wiki_clean_norep_main %>% 
  group_by(source, Social.Category, face_id, image_id) %>% 
  dplyr::summarise(img.gender.mode=getmode(img.gender), 
                   img.age.mode=getmode(img.age), 
                   img.age.avg=mean(img.age,na.rm=T))

ggplot(wiki_clean_norep_main_agg, aes(x = img.age.mode, fill=img.gender.mode, color=img.gender.mode, group=img.gender.mode, linetype=img.gender.mode)) + 
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
  scale_x_continuous(limits=c(-1,8), breaks=c(1,2,3,4,5,6,7),labels=c("0-11", "12-17","18-24","25-34","35-54","55-74","+75")) + 
  geom_vline(xintercept = mean(subset(wiki_clean_norep_main_agg, img.gender.mode=="Male")$img.age.mode), 
             color="blue", size=2, alpha=1, linetype="solid") + 
  geom_vline(xintercept = mean(subset(wiki_clean_norep_main_agg, img.gender.mode=="Female")$img.age.mode),
             color="goldenrod", size=2, alpha=1) 

t.test(subset(wiki_clean_norep_main_agg, img.gender.mode=="Female")$img.age.mode, 
       subset(wiki_clean_norep_main_agg, img.gender.mode=="Male")$img.age.mode)

wiki_main_rep<-subset(wiki_main, !face_id %in% wiki_clean_norep$face_id)
wiki_main_rep_agg<- wiki_main_rep %>% group_by(source, Social.Category, face_id, image_id) %>% 
  dplyr::summarise(img.gender.mode=getmode(img.gender), 
                   img.age.mode=getmode(img.age), 
                   img.age.avg=mean(img.age,na.rm=T))

t.test(subset(wiki_main_rep_agg, img.gender.mode=="Female")$img.age.mode, 
       subset(wiki_main_rep_agg, img.gender.mode=="Male")$img.age.mode)


##################################################################
#####Controlling for Number of Faces in Each Image################
##################################################################

#num faces (Google)#
google_clean_simp<-subset(google_clean, !img.gender %in% c("Non-binary"))
google_clean_wNumFaces<-google_clean_simp %>% group_by(image_id) %>% 
  dplyr::summarise(num_faces = length(unique(face_id)))
google_clean_wNumFaces_full<-merge(google_clean_simp, google_clean_wNumFaces, by=c("image_id"), all=T)

Google_mod_demo_wNumFace<-lm(img.age ~ img.gender + num_faces, data = google_clean_wNumFaces_full)
summary(Google_mod_demo_wNumFace)
Google_mod_demo_wNumFace_vcov <- cluster.vcov(Google_mod_demo_wNumFace, google_clean_wNumFaces_full$Social.Category)
coeftest(Google_mod_demo_wNumFace, Google_mod_demo_wNumFace_vcov)

#num faces (Wiki)#
wiki_clean_simp<-subset(wiki_clean, !img.gender %in% c("Non-binary"))
wiki_clean_wNumFaces<-wiki_clean_simp %>% group_by(image_id) %>% 
  dplyr::summarise(num_faces = length(unique(face_id)))
wiki_clean_wNumFaces_full<-merge(wiki_clean_simp, wiki_clean_wNumFaces, by=c("image_id"), all=T)

wiki_mod_demo_wNumFace<-lm(img.age ~ img.gender + num_faces, data = wiki_clean_wNumFaces_full)
summary(wiki_mod_demo_wNumFace)
wiki_mod_demo_wNumFace_vcov <- cluster.vcov(wiki_mod_demo_wNumFace, wiki_clean_wNumFaces_full$Social.Category)
coeftest(wiki_mod_demo_wNumFace, wiki_mod_demo_wNumFace_vcov)

############################################
#Controlling for frequency in Google Trends#
############################################
google_trends_freq<-read.csv("C:/Users/dougl/Desktop/comp-syn/algorithmic_bias/Data/RnR/Final_Submit/Google_Search_Frequency_data.csv")
google_main_wfreq<-merge(google_main, google_trends_freq, by=c("Social.Category"))

google_freq_mod<-lm(img.age ~ img.gender + Google.Img.US.Search.Freq, data=google_main_wfreq)
summary(google_freq_mod)
google_freq_vcov_category <- cluster.vcov(google_freq_mod, google_main_wfreq$Social.Category)
coeftest(google_freq_mod, google_freq_vcov_category)
google_freq_vcov_coder <- cluster.vcov(google_freq_mod, google_main_wfreq$WorkerId)
coeftest(google_freq_mod, google_freq_vcov_coder)

###############################
#Robustness aggregation method#
###############################
t.test(subset(google_main_agg, img.gender.mode == "Female")$img.age.avg, 
       subset(google_main_agg, img.gender.mode == "Male")$img.age.avg)

t.test(subset(google_gendered, img.gender.mode == "Female")$img.age.avg, 
       subset(google_gendered, img.gender.mode == "Male")$img.age.avg)

t.test(subset(wiki_main_agg, img.gender.mode == "Female")$img.age.avg, 
       subset(wiki_main_agg, img.gender.mode == "Male")$img.age.avg)

##########################################
#Robustness to examining uncropped images#
##########################################
uncropped<-read.csv("C:/Users/dougl/Desktop/comp-syn/algorithmic_bias/Data/RnR/Final_Submit/Google_data_uncropped_sample.csv")
uncropped_clean<-subset(uncropped, humface=="Yes" & Attention.Check & img.gender %in% c("Male", "Female"))

uncropped_stats<-uncropped_clean %>% group_by(img.gender) %>% 
  dplyr::summarise(avg_age=mean(img.age), modal_age=getmode(img.age))

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

validation_dt_full<-merge(validation_dt_age, validation_dt_gender, by=c("SubjID", "img_id"))
validation_dt_full<-merge(validation_dt_full, validation_dt_familiar, by=c("SubjID", "img_id"))
validation_dt_full_clean<-subset(validation_dt_full, familiar!="Yes")

table(validation_dt_full_clean$age_match)/sum(table(validation_dt_full_clean$age_match))
table(validation_dt_full_clean$mod_acc_r1)/sum(table(validation_dt_full_clean$mod_acc_r1))
table(validation_dt_full_clean$gender_match)/sum(table(validation_dt_full_clean$gender_match))

validation_dt_f<-subset(validation_dt_full_clean, real_gender=="Female")
validation_dt_m<-subset(validation_dt_full_clean, real_gender=="Male")
wilcox.test(validation_dt_f$real_age_num, validation_dt_m$real_age_num)

#validation robustness compare age assigned to genders within the same age group
validation_dt_full_clean$age_num<-as.numeric(as.character(validation_dt_full_clean$age_num))
validation_dt_full_clean_comp<-validation_dt_full_clean[complete.cases(validation_dt_full_clean),]

validation_dt_full_clean_comp_agg<-validation_dt_full_clean_comp %>% 
  group_by(img_id, real_age.x, real_gender.x) %>%
  dplyr::summarise(age_num = mean(age_num, na.rm=T))

validation_dt_full_clean_comp_agg %>% group_by(real_age.x) %>%
  dplyr::summarise(
    t=t.test(age_num[real_gender.x=="Male"], age_num[real_gender.x=="Female"])$statistic,
    p=round(t.test(age_num[real_gender.x=="Male"], age_num[real_gender.x=="Female"])$p.val,2),
    Male_age_num = mean(age_num[real_gender.x=="Male"], na.rm=T), 
    SD_Male_age_num = sd(age_num[real_gender.x=="Male"], na.rm=T), 
    Female_age_num = mean(age_num[real_gender.x=="Female"], na.rm=T),
    SD_Female_age_num = sd(age_num[real_gender.x=="Female"], na.rm=T),
    diff=Male_age_num-Female_age_num)

validation_wide<-merge(validation_dt_age, validation_dt_gender, 
                       by=c("SubjID","img_id", "real_age", "real_age_num","real_gender"))
validation_wide$age_num<-as.numeric(as.character(validation_wide$age_num))
validation_wide$SubjID<-as.factor(validation_wide$SubjID)
validation_wide_simp<-subset(validation_wide, !Guess.Gender %in% c("Non-binary"))
mod_within_age<-lm(age_num ~ real_age * Guess.Gender + SubjID, data=validation_wide_simp)
summary(mod_within_age)
user_vcov <- cluster.vcov(mod_within_age, validation_wide_simp$img_id)
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
google_clean_agg_simp<-subset(google_clean_agg, !img.gender.mode %in% c("Non-binary"))
numfaces<-length(unique(google_clean_agg_simp$face_id))
male_to_add<-round((numfaces*0.08) * 0.48)
female_to_add<-round((numfaces*0.08) * 0.52)
all_categories<-unique(google_clean_agg_simp$Social.Category)

google_clean_agg_aug<-google_clean_agg_simp
aug_male_cat<-sample(all_categories, male_to_add, replace=TRUE)
aug_male_age<-sample(c(1,2,3), male_to_add, replace=TRUE)
aug_male_df<-data.frame(source="Google", Social.Category=aug_male_cat, 
                        face_id = "aug_male_", image_id = "aug_male_", 
                        img.gender.mode = "Male", img.age.mode = aug_male_age, 
                        img.age.avg = aug_male_age)
aug_male_df$face_id<-paste(aug_male_df$face_id, 1:nrow(aug_male_df), sep="")
aug_male_df$image_id<-paste(aug_male_df$image_id, 1:nrow(aug_male_df), sep="")
google_clean_agg_aug<-rbind(google_clean_agg_aug, aug_male_df)

aug_female_cat<-sample(all_categories, female_to_add, replace=TRUE)
aug_female_age<-sample(c(5,6,7), female_to_add, replace=TRUE)
aug_female_df<-data.frame(source="Google", Social.Category=aug_female_cat, 
                          face_id = "aug_female_", image_id = "aug_female_", 
                          img.gender.mode = "Female", img.age.mode = aug_female_age, 
                          img.age.avg = aug_female_age)
aug_female_df$face_id<-paste(aug_female_df$face_id, 1:nrow(aug_female_df), sep="")
aug_female_df$image_id<-paste(aug_female_df$image_id, 1:nrow(aug_female_df), sep="")
google_clean_agg_aug<-rbind(google_clean_agg_aug, aug_female_df)

t.test(subset(google_clean_agg_aug, img.gender.mode=="Female")$img.age.mode, 
       subset(google_clean_agg_aug, img.gender.mode=="Male")$img.age.mode)

google_clean_agg_aug_cat<-google_clean_agg_aug %>% group_by(Social.Category, img.gender.mode) %>%
  dplyr::summarise(img.age.mode=mean(img.age.mode))

google_clean_agg_aug_cat$img.gender.mode<-as.factor(google_clean_agg_aug_cat$img.gender.mode)
google_clean_agg_aug %>% group_by(img.gender.mode) %>% dplyr::summarise(img.age.mode = mean(img.age.mode))

ggplot(google_clean_agg_aug, aes(x = img.age.mode, fill=img.gender.mode, color=img.gender.mode, group=img.gender.mode, linetype=img.gender.mode)) + 
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
                        img.gender.mode = "Male", img.age.mode = wiki_aug_male_age, 
                        img.age.avg = wiki_aug_male_age)
wiki_aug_male_df$face_id<-paste(wiki_aug_male_df$face_id, 1:nrow(wiki_aug_male_df), sep="")
wiki_aug_male_df$image_id<-paste(wiki_aug_male_df$image_id, 1:nrow(wiki_aug_male_df), sep="")
wiki_main_agg_aug<-rbind(wiki_main_agg_aug, wiki_aug_male_df)

wiki_aug_female_cat<-sample(all_categories, wiki_female_to_add, replace=TRUE)
wiki_aug_female_age<-sample(c(5, 6,7), wiki_female_to_add, replace=TRUE)
wiki_aug_female_df<-data.frame(source="Wikipedia", Social.Category=wiki_aug_female_cat, 
                          face_id = "aug_female_", image_id = "aug_female_", 
                          img.gender.mode = "Female", img.age.mode = wiki_aug_female_age, 
                          img.age.avg = wiki_aug_female_age)
wiki_aug_female_df$face_id<-paste(wiki_aug_female_df$face_id, 1:nrow(wiki_aug_female_df), sep="")
wiki_aug_female_df$image_id<-paste(wiki_aug_female_df$image_id, 1:nrow(wiki_aug_female_df), sep="")
wiki_main_agg_aug<-rbind(wiki_main_agg_aug, wiki_aug_female_df)

t.test(subset(wiki_main_agg_aug, img.gender.mode=="Female")$img.age.mode, 
       subset(wiki_main_agg_aug, img.gender.mode=="Male")$img.age.mode)

wiki_main_agg_aug_cat<-wiki_main_agg_aug %>% group_by(Social.Category, img.gender.mode) %>%
  dplyr::summarise(img.age.mode=mean(img.age.mode))

wiki_main_agg_aug_cat$img.gender.mode<-as.factor(wiki_main_agg_aug_cat$img.gender.mode)

ggplot(wiki_main_agg_aug, aes(x = img.age.mode, fill=img.gender.mode, color=img.gender.mode, group=img.gender.mode, linetype=img.gender.mode)) + 
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
male_dist<-subset(google_clean_agg, img.gender.mode=="Male")
female_dist<-subset(google_clean_agg, img.gender.mode=="Female")
female_dist$rowid<-1:nrow(female_dist)
female_dist_num<-nrow(female_dist)

#ONLY EXAMINING EFFECTS OF ADJUSTING HYPOTHETICAL BIAS WHEREBY CODERS UNDERPREDICT AGE OF YOUNG WOMEN#
female_dist_low<-subset(female_dist, img.age.mode<4)
female_dist_low$rowid<-1:nrow(female_dist_low)
female_dist_low_num<-nrow(female_dist_low)

female_dist_high<-subset(female_dist, img.age.mode>4)
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
    female_dist_low_mod$img.age.mode<-female_dist_low_mod$img.age.mode + 1
    female_dist_low_final<-rbind(female_dist_low_unmod, female_dist_low_mod)
    sim_data_final<-rbind(female_dist_high, female_dist_low_final)
    comparison<-t.test(sim_data_final$img.age.mode, male_dist$img.age.mode)
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
    female_dist_mod_low<-subset(female_dist_mod, img.age.mode<4)
    female_dist_mod_low$img.age.mode<-female_dist_mod_low$img.age.mode + 1
    female_dist_mod_high<-subset(female_dist_mod, img.age.mode>4)
    female_dist_mod_high$img.age.mode<-female_dist_mod_high$img.age.mode - 1
    female_dist_mod_final<-rbind(female_dist_mod_low, female_dist_mod_high)
    sim_data_final<-rbind(female_dist_unmod, female_dist_mod_final)
    comparison<-t.test(sim_data_final$img.age.mode, male_dist$img.age.mode)
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

