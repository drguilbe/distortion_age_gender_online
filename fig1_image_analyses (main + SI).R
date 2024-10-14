#Replication Script for Study of Gendered Ageism in online images 
#Douglas R. Guilbeault & Solene Delecourt
#Haas School of Business
#University of California, Berkeley

rm(list=ls());gc()
library(dplyr)
library(ggplot2)
library(tidyverse)
library(irrCAC) #https://search.r-project.org/CRAN/refmans/irrCAC/html/gwet.ac1.raw.html

#library(sjPlot) #https://strengejacke.github.io/sjPlot/articles/tab_mixed.html
#library(clinfun)
#library(ggridges);theme_set(theme_ridges())
#library(jtools) #https://cran.r-project.org/web/packages/jtools/vignettes/summ.html
#library(lmtest)
#library(multiwayvcov)
#library(rms)

#functions
min_max_norm<-function(x){(x - min(x,na.rm=T))/(max(x,na.rm=T) - min(x,na.rm=T))}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

savepath<-"G:/My Drive/Research/Labs/COMPSYN/gendered ageism/results/"

###########
#Load Data#
###########
data_raw<-read.csv("G:/My Drive/Research/Labs/COMPSYN/gendered ageism/data/gendered_ageism_img/nature_data_obsv.csv")
data_neu<-subset(data_raw, !searchDEMO %in% c("Male", "Female") & Data.Source == "Google")
data_gen<-subset(data_raw, searchDEMO %in% c("Male", "Female")  & Data.Source == "Google")
data_binary<-subset(data_raw, Img.Gender != "Non-binary")
data_main<-subset(data_binary, !searchDEMO %in% c("Male", "Female"))

data_clean_agg<- data_raw %>% 
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
IMDBceleb<-read.csv("G:/My Drive/Research/Labs/COMPSYN/gendered ageism/data/gendered_ageism_img/imdb_age_celeb.csv")
IMDBceleb<-subset(IMDBceleb, gender %in% c(0,1))
IMDBceleb<-subset(IMDBceleb, age > 0 & age <= 100)
IMDBceleb$gender_cat<-as.factor(IMDBceleb$gender)
levels(IMDBceleb$gender_cat)<-c("Female", "Male")

####################################
#Wikipedia Data (IMDb-Wiki Dataset)#
####################################
wikiceleb<-read.csv("G:/My Drive/Research/Labs/COMPSYN/gendered ageism/data/gendered_ageism_img/wiki_age_celeb.csv")
wikiceleb<-subset(wikiceleb, gender %in% c(0,1))
wikiceleb<-subset(wikiceleb, age > 0 & age <= 100)
wikiceleb$gender_cat<-as.factor(wikiceleb$gender)
levels(wikiceleb$gender_cat)<-c("Female", "Male")

############################
#Google Data (CACD Dataset)#
############################
CADC<-read.csv("G:/My Drive/Research/Labs/COMPSYN/gendered ageism/data/gendered_ageism_img/CACD.csv")
IMDBceleb_binary<-unique(IMDBceleb %>% select(name, gender))
CADC_comp<-merge(IMDBceleb_binary, CADC, by=c("name"))
CADC_comp$gender<-as.factor(CADC_comp$gender)
levels(CADC_comp$gender)<-c("Female", "Male")

#####################
#########LFW#########
#####################

LFW<-read.csv("G:/My Drive/Research/Labs/COMPSYN/gendered ageism/data/gendered_ageism_img/lfw.csv")

##############
#Main Results#
##############

################
####Figure 1####
################

#Fig. 1A#
google_main_agg<-subset(data_main_agg, Data.Source=="Google")
google_main_agg$Img.Gender.Mode<-as.factor(google_main_agg$Img.Gender.Mode)
google_main_agg_stats<-subset(data_main_agg_stats, Data.Source=="Google")

google_ungendered<-ggplot(google_main_agg, 
                          aes(x = Img.Age.Avg, group=Img.Gender.Mode)) + 
  theme_bw() + geom_density(lwd = 3, colour = "black", alpha=0.7, bw = 0.38, aes(fill=Img.Gender.Mode)) +
  scale_fill_manual(values=c("orange", "dodgerblue")) + 
  xlab("Age of Face") + ggtitle("Google\n(Guilbeault et al. 2024)") + 
  theme(legend.text=element_text(size=60),
        legend.position=c(0.85,0.88),
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.5, angle=28, vjust=0.7),
        axis.text.y=element_text(size = 60, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.ticks = element_line(size = 3), 
        axis.ticks.length = unit(0.3, "cm")) + 
  scale_x_continuous(limits=c(0,8), 
                     breaks=c(1,2,3,4,5,6,7),
                     labels=c("0-11", "12-17","18-24","25-34","35-54","55-74","+75")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  coord_cartesian(ylim=c(0,0.45)) +
  geom_vline(xintercept = subset(google_main_agg_stats, Img.Gender.Mode=="Female")$Avg.Img.Age.Num, color="orange", linewidth=6) + 
  geom_vline(xintercept = subset(google_main_agg_stats, Img.Gender.Mode=="Male")$Avg.Img.Age.Num, color="dodgerblue", linewidth=6) 

print(google_ungendered)
ggsave('Google_ungendered_searches.png', width=15, height=15, path = savepath)

pdf(file = paste0(savepath, 'Google_ungendered_searches.pdf'), width = 15, height = 15, useDingbats = FALSE)
print(google_ungendered)
dev.off()

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

google_gendered_agg_stats<-google_gendered_agg %>% group_by(Img.Gender.Mode) %>% 
  dplyr::summarise(Avg.Img.Age.Num = mean(Img.Age.Avg))

google_gendered<-ggplot(google_gendered_agg, aes(x = Img.Age.Avg, fill=Img.Gender.Mode, color=Img.Gender.Mode, group=Img.Gender.Mode)) + 
  theme_bw() + geom_density(lwd = 3, colour = "black", alpha = 0.7, bw = 0.38) +
  scale_fill_manual(values=c("orange", "dodgerblue")) + 
  xlab("Age of Face") + ggtitle("Google\n(Guilbeault et al. 2024 | Gendered)") + 
  theme(legend.text=element_text(size=60),
        legend.position=c(0.85,0.88),
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.5, angle=28, vjust=0.7),
        axis.text.y=element_text(size = 60, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.ticks = element_line(size = 3), 
        axis.ticks.length = unit(0.3, "cm")) + 
  scale_x_continuous(limits=c(2,6.2), 
                     breaks=c(2,3,4,5,6),
                     labels=c("12-17","18-24","25-34","35-54","55-74")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  #coord_cartesian(ylim=c(0,0.45)) + 
  geom_vline(xintercept = subset(google_gendered_agg_stats, Img.Gender.Mode=="Female")$Avg.Img.Age.Num, color="orange", linewidth=6) + 
  geom_vline(xintercept = subset(google_gendered_agg_stats, Img.Gender.Mode=="Male")$Avg.Img.Age.Num, color="dodgerblue", linewidth=6) 

print(google_gendered)
ggsave('Google_gendered_searches.png', width=15, height=15, path = savepath)

pdf(file = paste0(savepath, 'Google_gendered_searches.pdf'), width = 15, height = 15, useDingbats = FALSE)
print(google_gendered)
dev.off()

#Fig. 1B Stats#
t.test(subset(google_gendered_agg, Img.Gender.Mode=="Female")$Img.Age.Avg, 
       subset(google_gendered_agg, Img.Gender.Mode=="Male")$Img.Age.Avg)

google_gendered_agg_cat_match<-google_gendered_agg %>% group_by(Social.Category) %>%
  dplyr::summarise(fem.age.avg=mean(Img.Age.Avg[Img.Gender.Mode=="Female"],na.rm=T),
                   fem.freq = length(Img.Age.Avg[Img.Gender.Mode=="Female"]), 
                   mal.age.avg=mean(Img.Age.Avg[Img.Gender.Mode=="Male"],na.rm=T),
                   mal.freq = length(Img.Age.Avg[Img.Gender.Mode=="Male"]))

google_gendered_agg_cat_match<-google_gendered_agg_cat_match[complete.cases(google_gendered_agg_cat_match),]

t.test(google_gendered_agg_cat_match$fem.age.avg, 
       google_gendered_agg_cat_match$mal.age.avg,paired=T)

length(unique(google_gendered$face_id))
length(unique(google_gendered_agg_cat_match$Social.Category))

#Fig. 1C#
wiki_dt<-subset(data_raw, Data.Source == "Wikipedia")

wiki_main_agg<-subset(data_main_agg, Data.Source=="Wikipedia")
wiki_main_agg_stats<-subset(data_main_agg_stats, Data.Source=="Wikipedia")

wiki_2021<-ggplot(wiki_main_agg, aes(x = Img.Age.Avg, fill=Img.Gender.Mode, color=Img.Gender.Mode, group=Img.Gender.Mode)) + 
  theme_bw() + geom_density(lwd = 3, colour = "black", alpha = 0.7, bw = 0.38) +
  scale_fill_manual(values=c("orange", "dodgerblue")) + 
  xlab("Age of Face") + ggtitle("Wikipedia\n(Srinivasan et al. 2021)") + 
  theme(legend.text=element_text(size=60),
        legend.position=c(0.85,0.88),
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.5, angle=28, vjust=0.7),
        axis.text.y=element_text(size = 60, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.ticks = element_line(size = 3), 
        axis.ticks.length = unit(0.3, "cm")) + 
  scale_x_continuous(limits=c(-0.1,8), 
                     breaks=c(1,2,3,4,5,6,7),
                     labels=c("0-11", "12-17","18-24","25-34","35-54","55-74","+75")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  coord_cartesian(ylim=c(0,0.45)) + 
  geom_vline(xintercept = subset(wiki_main_agg_stats, Img.Gender.Mode=="Female")$Avg.Img.Age.Num, color="orange", linewidth=6) + 
  geom_vline(xintercept = subset(wiki_main_agg_stats, Img.Gender.Mode=="Male")$Avg.Img.Age.Num, color="dodgerblue", linewidth=6) 

print(wiki_2021)
ggsave('wiki_2021.png', width=15, height=15, path = savepath)

pdf(file = paste0(savepath, 'wiki_2021.pdf'), width = 15, height = 15, useDingbats = FALSE)
print(wiki_2021)
dev.off()

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

#################################################
##Replication with Ground-truth Classifications##
##################Figure 1#######################
#################################################

#Fig. 1D#
IMDb_Rothe<-ggplot(IMDBceleb, aes(x = age, fill=gender_cat, alpha=gender_cat)) +
  theme_bw() + geom_density(lwd = 3, colour = "black", alpha = 0.7, bw = 0.38, adjust=2) +
  scale_fill_manual(values=c("orange", "dodgerblue")) + 
  xlab("Age of Face") + ggtitle("IMDb\n(Rothe et al. 2018)") +
  theme(legend.text=element_text(size=60),
        legend.position=c(0.85,0.88),
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.5, angle=28, vjust=0.7),
        axis.text.y=element_text(size = 60, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.ticks = element_line(size = 3), 
        axis.ticks.length = unit(0.3, "cm")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  scale_x_continuous(limits=c(0,100), breaks=c(0,10,20,30,40,50,60,70,80,90,100)) + 
  geom_vline(xintercept = mean(subset(IMDBceleb, gender_cat=="Female")$age, na.rm=T), color="orange", size=6) + 
  geom_vline(xintercept = mean(subset(IMDBceleb, gender_cat=="Male")$age, na.rm=T), color="blue", size=6)

print(IMDb_Rothe)
ggsave('IMDb_Rothe.png', width=15, height=15, path = savepath)

pdf(file = paste0(savepath, 'IMDb_Rothe.pdf'), width = 15, height = 15, useDingbats = FALSE)
print(IMDb_Rothe)
dev.off()

imdb_female=subset(IMDBceleb, gender==0)
imdb_male=subset(IMDBceleb, gender==1)
t.test(imdb_female$age, imdb_male$age)

#Fig. 1E#
wiki_Rothe<-ggplot(wikiceleb, aes(x = age, fill=gender_cat, alpha=gender_cat)) +
  theme_bw() + geom_density(lwd = 3, colour = "black", alpha = 0.7, bw = 0.38, adjust=2) +
  scale_fill_manual(values=c("orange", "dodgerblue")) + 
  xlab("Age of Face") + ggtitle("Wikipedia\n(Rothe et al. 2018)") + 
  theme(legend.text=element_text(size=60),
        legend.position=c(0.85,0.88),
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.5, angle=28, vjust=0.7),
        axis.text.y=element_text(size = 60, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.ticks = element_line(size = 3), 
        axis.ticks.length = unit(0.3, "cm")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  scale_x_continuous(limits=c(10,100), breaks=c(0,10,20,30,40,50,60,70,80,90,100)) + 
  geom_vline(xintercept = mean(subset(wikiceleb, gender_cat=="Female")$age, na.rm=T), color="orange", size=6) + 
  geom_vline(xintercept = mean(subset(wikiceleb, gender_cat=="Male")$age, na.rm=T), color="blue", size=6)

print(wiki_Rothe)
ggsave('wiki_Rothe.png', width=15, height=15, path = savepath)

pdf(file = paste0(savepath, 'wiki_Rothe.pdf'), width = 15, height = 15, useDingbats = FALSE)
print(wiki_Rothe)
dev.off()

wiki_female=subset(wikiceleb, gender==0)
wiki_male=subset(wikiceleb, gender==1)
t.test(wiki_female$age, wiki_male$age)

#Fig. 1F#
CADC_fig<-ggplot(CADC_comp, aes(x = age, fill=gender, alpha=gender)) +
  theme_bw() + geom_density(lwd = 3, colour = "black", alpha = 0.7, bw = 0.38, adjust=2) +
  scale_fill_manual(values=c("orange", "dodgerblue")) + 
  xlab("Age of Face") + ggtitle("Google\n(Chen et al. 2014)") +
  theme(legend.text=element_text(size=60),
        legend.position=c(0.85,0.92),
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.5, angle=28, vjust=0.7),
        axis.text.y=element_text(size = 60, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.ticks = element_line(size = 3), 
        axis.ticks.length = unit(0.3, "cm")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  scale_x_continuous(limits=c(12,64), breaks=c(0,10,20,30,40,50,60,70,80,90,100)) + 
  geom_vline(xintercept = mean(subset(CADC_comp, gender=="Female")$age, na.rm=T), color="orange", size=6) + 
  geom_vline(xintercept = mean(subset(CADC_comp, gender=="Male")$age, na.rm=T), color="blue", size=6)

print(CADC_fig)
ggsave('CADC.png', width=15, height=15, path = savepath)

pdf(file = paste0(savepath, 'CADC.pdf'), width = 15, height = 15, useDingbats = FALSE)
print(CADC_fig)
dev.off()

t.test(subset(CADC_comp, gender=="Female")$age, 
       subset(CADC_comp, gender=="Male")$age)

###LFW 

LFW_fig<-ggplot(LFW, aes(x = age_bin, fill=gender, color=gender, group=gender)) + 
  theme_bw() + geom_density(lwd = 3, colour = "black", alpha = 0.7, bw = 0.38, adjust=2) +
  scale_fill_manual(values=c("orange", "dodgerblue")) + 
  scale_linetype_manual(values=c("dotted", "solid")) + 
  xlab("Age of Face") + ggtitle("Labeled Faces in the Wild\n(Huang et al. 2008)") + 
  theme(legend.text=element_text(size=60),
        legend.position=c(0.85,0.88),
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.5, angle=28, vjust=0.7),
        axis.text.y=element_text(size = 60, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(1,2,3,4,5),
                     labels=c("baby", "child", "teen", "adult", "senior")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  geom_vline(xintercept = mean(subset(LFW, gender=="Female")$age_bin, na.rm=T), color="orange", size=6) + 
  geom_vline(xintercept = mean(subset(LFW, gender=="Male")$age_bin, na.rm=T), color="blue", size=6)

print(LFW_fig)
ggsave('LFW.png', width=15, height=15, path = savepath)

pdf(file = paste0(savepath, 'LFW.pdf'), width = 15, height = 15, useDingbats = FALSE)
print(LFW_fig)
dev.off()

cor.test(LFW$male, LFW$age_bin)

t.test(subset(LFW, !male_cat)$age_bin, subset(LFW, male_cat)$age_bin)

################
#IP Replication#
####Figure 2####
################
IP_data<-read.csv("G:/My Drive/Research/Labs/COMPSYN/gendered ageism/data/gendered_ageism_img/nature_data_supp/GEO_replication.csv")
IP_simp<-subset(IP_data, gender %in% c("Male", "Female"))
IP_data_clean<-subset(IP_data, Attention.Check & gender %in% c("Male","Female") & humface=="Yes")
IP_data_clean$Img.Age<-as.factor(IP_data_clean$age)
levels(IP_data_clean$Img.Age)<-c(7,1,2,3,4,5,6)
IP_data_clean$Img.Age<-as.numeric(as.character(IP_data_clean$Img.Age))

IP_avg<-IP_data_clean %>% group_by(IP, gender) %>% 
  dplyr::summarise(avg_age=mean(Img.Age, na.rm=T), 
                   mode_age=getmode(Img.Age), 
                   median_age=getmode(Img.Age))

gen_age_stat<-IP_data_clean %>% group_by(IP) %>% 
  dplyr::summarise(t = t.test(Img.Age[gender=="Female"], Img.Age[gender=="Male"])$statistic, 
                   p = t.test(Img.Age[gender=="Female"], Img.Age[gender=="Male"])$p.value,
                   avgFemale=t.test(Img.Age[gender=="Female"], Img.Age[gender=="Male"])$estimate[1], 
                   avgMale=t.test(Img.Age[gender=="Female"], Img.Age[gender=="Male"])$estimate[2], 
                   diff=avgMale - avgFemale,
                   num_imgs=length(unique(face_id)))

IP_data_clean$IP<-as.factor(IP_data_clean$IP)
levels(IP_data_clean$IP)<-c("Amsterdam","Bangalore","Frankfurt","Singapore", "Toronto")

IP_data_clean$IP_gender<-paste(IP_data_clean$IP, IP_data_clean$gender, sep="_")
IP_data_clean$IP_gender<-as.factor(IP_data_clean$IP_gender)
levels(IP_data_clean$IP_gender)

ggplot(subset(IP_data_clean, IP == "Toronto"), aes(x = Img.Age, group=gender)) + 
  theme_bw() + geom_density(lwd = 3, colour = "black", alpha=0.7, bw = 0.38, aes(fill=gender)) +
  scale_fill_manual(values=c("orange", "dodgerblue")) + 
  xlab("Age of Face") + ggtitle("Toronto") + 
  theme(legend.text=element_text(size=60),
        legend.position=c(0.85,0.88),
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.5, angle=28, vjust=0.7),
        axis.text.y=element_text(size = 60, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.ticks = element_line(size = 3), 
        axis.ticks.length = unit(0.3, "cm")) + 
  scale_x_continuous(limits=c(0,8), 
                     breaks=c(1,2,3,4,5,6,7),
                     labels=c("0-11", "12-17","18-24","25-34","35-54","55-74","+75")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  coord_cartesian(ylim=c(0,0.45)) +
  geom_vline(xintercept = mean(subset(IP_data_clean, IP == "Toronto" & gender == "Female")$Img.Age), color="orange", linewidth=6) + 
  geom_vline(xintercept = mean(subset(IP_data_clean, IP == "Toronto" & gender == "Male")$Img.Age), color="dodgerblue", linewidth=6) 

ggsave('Toronto.png', width=15, height=15, path = savepath)

t.test(subset(IP_data_clean, IP == "Toronto" & gender== "Female")$Img.Age, 
       subset(IP_data_clean, IP == "Toronto" & gender== "Male")$Img.Age)

ggplot(subset(IP_data_clean, IP == "Frankfurt"), aes(x = Img.Age, group=gender)) + theme_bw() + 
  geom_density(lwd = 3, colour = "black", alpha=0.7, bw = 0.38, adjust=1.1,aes(fill=gender)) +
  scale_fill_manual(values=c("orange", "dodgerblue")) + 
  xlab("Age of Face") + ggtitle("Frankfurt") + 
  theme(legend.text=element_text(size=60),
        legend.position=c(0.85,0.88),
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.5, angle=28, vjust=0.7),
        axis.text.y=element_text(size = 60, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.ticks = element_line(size = 3), 
        axis.ticks.length = unit(0.3, "cm")) + 
  scale_x_continuous(limits=c(0,8), 
                     breaks=c(1,2,3,4,5,6,7),
                     labels=c("0-11", "12-17","18-24","25-34","35-54","55-74","+75")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  coord_cartesian(ylim=c(0,0.45)) +
  geom_vline(xintercept = mean(subset(IP_data_clean, IP == "Frankfurt" & gender == "Female")$Img.Age), color="orange", linewidth=6) + 
  geom_vline(xintercept = mean(subset(IP_data_clean, IP == "Frankfurt" & gender == "Male")$Img.Age), color="dodgerblue", linewidth=6) 

ggsave('Frankfurt.png', width=15, height=15, path = savepath)

t.test(subset(IP_data_clean, IP == "Frankfurt" & gender== "Female")$Img.Age, 
       subset(IP_data_clean, IP == "Frankfurt" & gender== "Male")$Img.Age)

ggplot(subset(IP_data_clean, IP == "Bangalore"), aes(x = Img.Age, group=gender)) + theme_bw() + 
  geom_density(lwd = 3, colour = "black", alpha=0.7, bw = 0.38, adjust=1.05,aes(fill=gender)) +
  scale_fill_manual(values=c("orange", "dodgerblue")) + 
  xlab("Age of Face") + ggtitle("Bangalore") + 
  theme(legend.text=element_text(size=60),
        legend.position=c(0.85,0.88),
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.5, angle=28, vjust=0.7),
        axis.text.y=element_text(size = 60, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.ticks = element_line(size = 3), 
        axis.ticks.length = unit(0.3, "cm")) + 
  scale_x_continuous(limits=c(0,8), 
                     breaks=c(1,2,3,4,5,6,7),
                     labels=c("0-11", "12-17","18-24","25-34","35-54","55-74","+75")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  coord_cartesian(ylim=c(0,0.45)) +
  geom_vline(xintercept = mean(subset(IP_data_clean, IP == "Bangalore" & gender == "Female")$Img.Age), color="orange", linewidth=6) + 
  geom_vline(xintercept = mean(subset(IP_data_clean, IP == "Bangalore" & gender == "Male")$Img.Age), color="dodgerblue", linewidth=6) 

ggsave('Bangalore.png', width=15, height=15, path = savepath)

t.test(subset(IP_data_clean, IP == "Bangalore" & gender== "Female")$Img.Age, 
       subset(IP_data_clean, IP == "Bangalore" & gender== "Male")$Img.Age)

ggplot(subset(IP_data_clean, IP == "Singapore"), aes(x = Img.Age, group=gender)) + theme_bw() + 
  geom_density(lwd = 3, colour = "black", alpha=0.7, bw = 0.38, adjust=1.15,aes(fill=gender)) +
  scale_fill_manual(values=c("orange", "dodgerblue")) + 
  xlab("Age of Face") + ggtitle("Singapore") + 
  theme(legend.text=element_text(size=60),
        legend.position=c(0.85,0.88),
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.5, angle=28, vjust=0.7),
        axis.text.y=element_text(size = 60, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.ticks = element_line(size = 3), 
        axis.ticks.length = unit(0.3, "cm")) + 
  scale_x_continuous(limits=c(0,8), 
                     breaks=c(1,2,3,4,5,6,7),
                     labels=c("0-11", "12-17","18-24","25-34","35-54","55-74","+75")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  coord_cartesian(ylim=c(0,0.45)) +
  geom_vline(xintercept = mean(subset(IP_data_clean, IP == "Singapore" & gender == "Female")$Img.Age), color="orange", linewidth=6) + 
  geom_vline(xintercept = mean(subset(IP_data_clean, IP == "Singapore" & gender == "Male")$Img.Age), color="dodgerblue", linewidth=6) 

ggsave('Singapore.png', width=15, height=15, path = savepath)

t.test(subset(IP_data_clean, IP == "Singapore" & gender== "Female")$Img.Age, 
       subset(IP_data_clean, IP == "Singapore" & gender== "Male")$Img.Age)

ggplot(subset(IP_data_clean, IP == "Amsterdam"), aes(x = Img.Age, group=gender)) + theme_bw() + 
  geom_density(lwd = 3, colour = "black", alpha=0.7, bw = 0.38, adjust=1.3,aes(fill=gender)) +
  scale_fill_manual(values=c("orange", "dodgerblue")) + 
  xlab("Age of Face") + ggtitle("Amsterdam") + 
  theme(legend.text=element_text(size=60),
        legend.position=c(0.85,0.88),
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.5, angle=28, vjust=0.7),
        axis.text.y=element_text(size = 60, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.ticks = element_line(size = 3), 
        axis.ticks.length = unit(0.3, "cm")) + 
  scale_x_continuous(limits=c(0,8), 
                     breaks=c(1,2,3,4,5,6,7),
                     labels=c("0-11", "12-17","18-24","25-34","35-54","55-74","+75")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  coord_cartesian(ylim=c(0,0.45)) +
  geom_vline(xintercept = mean(subset(IP_data_clean, IP == "Amsterdam" & gender == "Female")$Img.Age), color="orange", linewidth=6) + 
  geom_vline(xintercept = mean(subset(IP_data_clean, IP == "Amsterdam" & gender == "Male")$Img.Age), color="dodgerblue", linewidth=6) 

ggsave('Amsterdam.png', width=15, height=15, path = savepath)

t.test(subset(IP_data_clean, IP == "Amsterdam" & gender== "Female")$Img.Age, 
       subset(IP_data_clean, IP == "Amsterdam" & gender== "Male")$Img.Age)

IP_data_clean %>% group_by(IP) %>% dplyr::summarise(numimgs = length(unique(face_id)))

####################################################
#Chance-corrected measures of inter-rater agreement#
####################################################

#gender#
dt_gender_rater<-data_main %>% dplyr::select(WorkerId, Social.Category, face_id, Img.Gender)
dt_gender_rater$coder_ID<-paste(dt_gender_rater$WorkerId, dt_gender_rater$face_id, sep="_")
dt_gender_rater_clean<-dt_gender_rater %>% group_by(coder_ID, face_id) %>% 
  dplyr::summarise(WorkerId = unique(WorkerId), Img.Gender = sample(Img.Gender, 1))
dt_gender_rater_clean<-dt_gender_rater_clean[,!names(dt_gender_rater_clean) %in% c("coder_ID")]

dt_gender_rater_matrix<-pivot_wider(dt_gender_rater_clean, names_from = WorkerId, values_from = Img.Gender)
dt_gender_rater_matrix<-as.data.frame(dt_gender_rater_matrix)
dt_gender_rater_matrix[dt_gender_rater_matrix == "NULL"] = NA
dt_gender_rater_gewt<-gwet.ac1.raw(dt_gender_rater_matrix)
dt_gender_rater_gewt$est

#age#
dt_age_rater<-data_main %>% dplyr::select(WorkerId, Social.Category, face_id, Img.Age.Cat)
dt_age_rater$coder_ID<-paste(dt_age_rater$WorkerId, dt_age_rater$face_id, sep="_")
dt_age_rater_clean<-dt_age_rater %>% group_by(coder_ID, face_id) %>% 
  dplyr::summarise(WorkerId = unique(WorkerId), Img.Age.Cat = sample(Img.Age.Cat, 1))
dt_age_rater_clean<-dt_age_rater_clean[,!names(dt_age_rater_clean) %in% c("coder_ID")]

dt_age_rater_matrix<-pivot_wider(dt_age_rater_clean, names_from = WorkerId, values_from = Img.Age.Cat)
dt_age_rater_matrix<-as.data.frame(dt_age_rater_matrix)
dt_age_rater_matrix[dt_age_rater_matrix == "NULL"] = NA
dt_age_rater_gewt<-gwet.ac1.raw(dt_age_rater_matrix) #can't allocate vector of this size

rand_sub<-dt_age_rater_matrix[sample(1:nrow(dt_age_rater_matrix), 10000),]

dt_age_rater_gewt<-gwet.ac1.raw(rand_sub)
dt_age_rater_gewt$est

#write code that iterates over and calculates intercoder reliability across many samples 


