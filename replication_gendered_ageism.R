#Replication Script for "Online Images from Google and Wikipedia Depict Women as much Younger than Men" 
#May 2022
#Douglas R. Guilbeault
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

#functions
min_max_norm<-function(x){(x - min(x,na.rm=T))/(max(x,na.rm=T) - min(x,na.rm=T))}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

####################
#Load/Organize Data#
####################
datapath<-"C:/Users/dougl/Desktop/comp-syn/algorithmic_bias/Data/"
google<-read.csv(paste(datapath,"google_dt_final.csv", sep=""))
google_clean<-subset(google, Attention.Check)
google_main<-subset(google_clean, searchDEMO == "None" & gender %in% c("Male","Female"))
wiki<-read.csv(paste(datapath,"wiki_dt_final.csv", sep=""))
wiki_clean<-subset(wiki, Attention.Check)
wiki_main<-subset(wiki_clean, gender %in% c("Male","Female"))

###################################
savepath<-"C:/Users/dougl/Desktop/"

#############
#Google Data#
#############
Google_Age_Stats<-google_main %>% group_by(gender) %>% 
  dplyr::summarise(Avg.Google.Img.Age.Num = mean(age_bin),
                   Med.Google.Img.Age.Num = median(age_bin),
                   Mode.Google.Img.Age.Num = getmode(age_bin))

#########
#Fig. 2A#
#########
ggplot(google_main, aes(x = age_bin, fill=gender, color=gender, group=gender)) + theme_bw() + 
  geom_density(lwd = 2, colour = "black", alpha = 0.7, bw = 0.7) +
  scale_fill_manual(values=c("goldenrod", "lightblue")) + 
  xlab("Age of Face") + ggtitle("Google") + 
  theme(legend.text=element_text(size=40),
        legend.position=c(0.85,0.88),
        legend.title = element_blank(),
        plot.title=element_text(size = 40, hjust = 0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size = 40, hjust = 0.5),
        axis.text.x=element_text(size = 30, hjust = 0.5, angle=37, vjust=0.7),
        axis.text.y=element_text(size = 40, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits=c(-1,8.8), 
                     breaks=c(1,2,3,4,5,6,7),
                     labels=c("0-11", "12-17","18-24","25-34","35-54","55-74","+75")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  coord_cartesian(ylim=c(0,0.35)) + 
  geom_vline(xintercept = subset(Google_Age_Stats, gender=="Female")$Mode.Google.Img.Age.Num, 
             color="goldenrod", size=3, alpha=0.8) + 
  geom_vline(xintercept = subset(Google_Age_Stats, gender=="Male")$Mode.Google.Img.Age.Num, 
             color="blue", size=3, alpha=0.8) 

t.test(subset(google_main, gender=="Female")$age_bin, subset(google_main, gender=="Male")$age_bin)
wilcox.test(subset(google_main, gender=="Female")$age_bin, subset(google_main, gender=="Male")$age_bin)

#Match by category
google_main_cat_match<-google_main %>% group_by(Social.Category) %>%
  dplyr::summarise(fem.age.avg=mean(age_bin[gender=="Female"],na.rm=T),
                   fem.freq = length(age_bin[gender=="Female"]), 
                   mal.age.avg=mean(age_bin[gender=="Male"],na.rm=T), 
                   mal.freq = length(age_bin[gender=="Male"]), 
                   search.freq = mean(Google.Img.US.Search.Freq))

google_main_cat_match<-google_main_cat_match[complete.cases(google_main_cat_match),]
t.test(google_main_cat_match$fem.age.avg, google_main_cat_match$mal.age.avg,paired=T)
wilcox.test(google_main_cat_match$fem.age.avg, google_main_cat_match$mal.age.avg,paired=T)

####################
#Wikipedia analysis# 
####################
Wiki_Age_Stats<-wiki_main %>% group_by(gender) %>% 
  dplyr::summarise(Avg.Wiki.Img.Age.Num = mean(age_bin),
                   Med.Wiki.Img.Age.Num = median(age_bin),
                   Mode.Wiki.Img.Age.Num = getmode(age_bin))

#########
#Fig. 2B#
#########
ggplot(wiki_main, aes(x = age_bin, fill=gender, color=age_bin, group=gender)) + theme_bw() + 
  geom_density(lwd = 2, colour = "black", alpha = 0.7, bw = 0.7) +
  scale_fill_manual(values=c("goldenrod", "lightblue")) + 
  xlab("Age of Face") + ggtitle("Wikipedia") + 
  theme(legend.text=element_text(size=40),
        legend.position=c(0.85,0.88),
        legend.title = element_blank(),
        plot.title=element_text(size = 40, hjust = 0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size = 40, hjust = 0.5),
        axis.text.x=element_text(size = 30, hjust = 0.5, angle=37, vjust=0.7),
        axis.text.y=element_text(size = 40, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits=c(-1,9.5), 
                     breaks=c(1,2,3,4,5,6,7),
                     labels=c("0-11", "12-17","18-24","25-34","35-54","55-74","+75")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  coord_cartesian(ylim=c(0,0.35)) + 
  geom_vline(xintercept = subset(Wiki_Age_Stats, gender=="Female")$Mode.Wiki.Img.Age.Num, 
             color="goldenrod", size=3, alpha=0.8) + 
  geom_vline(xintercept = subset(Wiki_Age_Stats, gender=="Male")$Mode.Wiki.Img.Age.Num, 
             color="blue", size=3, alpha=0.8)

t.test(subset(wiki_main, gender=="Female")$age_bin, subset(wiki_main, gender=="Male")$age_bin)
wilcox.test(subset(wiki_main, gender=="Female")$age_bin, subset(wiki_main, gender=="Male")$age_bin)

#Match by category
wiki_main_cat_match<-wiki_main %>% group_by(Social.Category) %>%
  dplyr::summarise(fem.age.avg=mean(age_bin[gender=="Female"],na.rm=T),
                   fem.freq = length(age_bin[gender=="Female"]), 
                   mal.age.avg=mean(age_bin[gender=="Male"],na.rm=T),
                   mal.freq = length(age_bin[gender=="Male"]))

wiki_main_cat_match<-wiki_main_cat_match[complete.cases(wiki_main_cat_match),]
t.test(wiki_main_cat_match$fem.age.avg, wiki_main_cat_match$mal.age.avg,paired=T)
wilcox.test(wiki_main_cat_match$fem.age.avg, wiki_main_cat_match$mal.age.avg,paired=T)

##########################
#Gendered Search Analysis#
##########################
google_gendered<-subset(google_clean, searchDEMO != "None" & gender %in% c("Male","Female"))
nrow(google_gendered)

google_gendered_agg<-google_gendered %>% group_by(Social.Category, gender) %>% 
  dplyr::summarise(age_bin = mean(age_bin), num_faces = length(unique(face_id)))

mean(subset(google_gendered_agg, gender=="Female")$num_faces)
mean(subset(google_gendered_agg, gender=="Male")$num_faces)

google_gendered_agg$ylabs<-as.factor(google_gendered_agg$gender)
levels(google_gendered_agg$ylabs)<-c("'Female'\nSearch", "'Male'\nSearch")

#########
#Fig. 2C#
#########
ggplot(google_gendered_agg, aes(x = age_bin, y = ylabs, fill=ylabs)) + 
  theme_bw() + 
  geom_boxplot(size=1, outlier.size= 0.5) + scale_fill_manual(values=c("goldenrod", "lightblue")) + 
  xlab("Age of Face") + 
  theme(legend.text=element_text(size=30),
        legend.position="none",
        legend.title = element_blank(),
        plot.title=element_blank(),axis.title.y=element_blank(),
        axis.title.x=element_text(size = 18, hjust = 0.5),
        axis.text.x=element_text(size = 18, hjust = 0.6, angle=0, vjust=0.7),
        axis.text.y=element_text(size = 18, hjust = 0.6, angle=90),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits=c(1,7), 
                     breaks=c(1,2,3,4,5,6,7),
                     labels=c("0-11", "12-17","18-24","25-34","35-54","55-74","+75")) 

google_gendered_agg_cat_match<-google_gendered_agg %>% group_by(Social.Category) %>%
  dplyr::summarise(fem.age.avg=mean(age_bin[gender=="Female"],na.rm=T),
                   fem.freq = length(age_bin[gender=="Female"]), 
                   mal.age.avg=mean(age_bin[gender=="Male"],na.rm=T),
                   mal.freq = length(age_bin[gender=="Male"]))

google_gendered_agg_cat_match<-google_gendered_agg_cat_match[complete.cases(google_gendered_agg_cat_match),]

t.test(google_gendered_agg_cat_match$fem.age.avg, google_gendered_agg_cat_match$mal.age.avg,paired=T)
wilcox.test(google_gendered_agg_cat_match$fem.age.avg, google_gendered_agg_cat_match$mal.age.avg,paired=T)

##################
#Robustness Model#
##################
mturk_demo<-read.csv(paste(datapath, file="mturk_demo.csv", sep=""), na.string ="")
google_main_wdemo<-merge(google_main, mturk_demo, by="WorkerId", all = TRUE)
google_main_wdemo<-subset(google_main_wdemo, !is.na(Social.Category))
google_main_wdemo$mt_age<-as.numeric(as.character(google_main_wdemo$mt_age))
google_main_wdemo$mt_income<-as.numeric(as.character(google_main_wdemo$mt_income))
google_main_wdemo$mt_education<-as.numeric(as.character(google_main_wdemo$mt_education))

google_main_wdemo_cat_match<-google_main_wdemo %>% group_by(Social.Category) %>%
  dplyr::summarise(fem.age.avg=mean(age_bin[gender=="Female"],na.rm=T),
                   fem.freq = length(age_bin[gender=="Female"]), 
                   mal.age.avg=mean(age_bin[gender=="Male"],na.rm=T), 
                   mal.freq = length(age_bin[gender=="Male"]), 
                   search.freq = mean(Google.Img.US.Search.Freq), 
                   mt_age = mean(mt_age, na.rm=T), 
                   mt_income = mean(mt_income, na.rm=T), 
                   mt_education = mean(mt_education, na.rm=T), 
                   mt_frac_liberal = sum(mt_poli.orient=="Liberal")/length(mt_poli.orient),
                   mt_Male = sum(mt_gender=="Male"), 
                   mt_Female = sum(mt_gender=="Female"))

google_main_wdemo_cat_match[is.na(google_main_wdemo_cat_match$mt_frac_liberal),]$mt_frac_liberal<-0
google_main_wdemo_cat_match[is.na(google_main_wdemo_cat_match$mt_Male),]$mt_Male<-0
google_main_wdemo_cat_match[is.na(google_main_wdemo_cat_match$mt_Female),]$mt_Female<-0

t.test(google_main_wdemo_cat_match$fem.age.avg, google_main_wdemo_cat_match$mal.age.avg,paired=T)
wilcox.test(google_main_wdemo_cat_match$fem.age.avg, google_main_wdemo_cat_match$mal.age.avg,paired=T)

Google_fem<-google_main_wdemo_cat_match %>% select(-mal.age.avg,-mal.freq) #swap with google_main_cat_match
colnames(Google_fem)<-c("Social.Category","Age.Avg","Img.Freq", "search.freq", "mt_age", "mt_income", "mt_education", "mt_frac_liberal", "mt_Male", "mt_Female")
Google_fem$Gender<-"Female"
Google_mal<-google_main_wdemo_cat_match %>% select(-fem.age.avg, -fem.freq)
colnames(Google_mal)<-c("Social.Category","Age.Avg","Img.Freq", "search.freq", "mt_age", "mt_income", "mt_education", "mt_frac_liberal", "mt_Male", "mt_Female")
Google_mal$Gender<-"Male"

Google_combo<-rbind(Google_fem,Google_mal)
Google_combo$Gender<-as.factor(Google_combo$Gender)
Google_combo <- within(Google_combo, Gender <- relevel(Gender, ref = 2))
Google_combo$search.freq.scaled<-Google_combo$search.freq/sd(Google_combo$search.freq, na.rm=T)

mod<-lm(Age.Avg ~ Gender + Img.Freq + search.freq.scaled + mt_age + mt_income + mt_education + mt_Female, data = Google_combo)
summary(mod)

robust_mod<-plot_summs(mod, robust = TRUE, plot.distributions = TRUE)
plot_robust<-robust_mod$data
levels(plot_robust$term)<-c("% Female Coders", "Avg. Coder Education", "Avg. Coder Income", 
                         "Avg. Coder Age", "Freq. Cat. in Google Search","Freq. Female in Cat.", "Female Face")

ggplot(plot_robust, aes(x = estimate, y=term, xmin=conf.low, xmax=conf.high)) + theme_bw() + 
  geom_point(size=9)+ geom_errorbar(width=0, size=1)+ 
  xlab("Age of Face") + ggtitle("Partial Correlations") + 
  theme(legend.text=element_text(size=40),
        legend.position="none",
        legend.title = element_blank(),
        plot.title=element_text(size = 45, hjust = 0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size = 45, hjust = 0.5),
        axis.text.x=element_text(size = 45, hjust = 0.5, vjust=0.7),
        axis.text.y=element_text(size = 45, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  geom_vline(xintercept = 0, size=1) + 
  scale_x_continuous(breaks=c(-0.75, -0.5, -0.25, 0),
                     labels=c("-0.75\n(18-24)","-0.5", "-0.25", "0\n(25-34)"))


