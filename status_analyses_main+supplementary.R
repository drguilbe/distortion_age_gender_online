rm(list=ls());gc()
library(dplyr); library(ggplot2); library(tidyverse);
library(lubridate); library(sjPlot); library(sjmisc)
library(lmtest); library(sandwich); library(gtsummary)
library(flextable); library(reshape2)
theme_set(theme_sjplot())

################
#Get Image Data#
################
data_raw<-read.csv("G:/My Drive/Research/Labs/COMPSYN/gendered ageism/data/gendered_ageism_img/nature_data_obsv.csv")
data_google_main<-subset(data_raw, !searchDEMO %in% c("Male", "Female") & Img.Gender != "Non-binary" & Data.Source == "Google")

data_google_agg<- data_google_main %>% 
  group_by(Data.Source, Social.Category, searchDEMO, face_id, image_id) %>% 
  dplyr::summarise(Img.Gender.Mode=getmode(Img.Gender), 
                   Img.Age.Mode=getmode(Img.Age), 
                   Img.Age.Avg=mean(Img.Age,na.rm=T))

data_google_agg_bygen<- data_google_agg %>% 
  group_by(Data.Source, Social.Category, Img.Gender.Mode) %>% 
  dplyr::summarise(Img.Age.Avg=mean(Img.Age.Avg,na.rm=T)) %>% 
  group_by(Social.Category) %>% mutate(numGen = length(unique(Img.Gender.Mode)))

data_google_agg_bygen_clean<-subset(data_google_agg_bygen, numGen==2)

data_google_agg_bygen_clean_gap<-data_google_agg_bygen_clean %>% 
  group_by(Social.Category) %>% 
  dplyr::summarise(male_older = Img.Age.Avg[Img.Gender.Mode=="Male"]>Img.Age.Avg[Img.Gender.Mode=="Female"], 
                   raw_age_gap = Img.Age.Avg[Img.Gender.Mode=="Male"]-Img.Age.Avg[Img.Gender.Mode=="Female"],
                   abs_age_gap = abs(raw_age_gap))

data_google_agg_propgen<- data_google_agg %>% 
  group_by(Data.Source, Social.Category, Img.Gender.Mode) %>% 
  dplyr::summarise(Img.Age.Avg=mean(Img.Age.Avg,na.rm=T), 
                   numfaces = length(unique(face_id))) %>% 
  group_by(Data.Source, Social.Category) %>% 
  dplyr::mutate(propfaces = numfaces/sum(numfaces))

data_google_agg_propmale<-subset(data_google_agg_propgen, Img.Gender.Mode == "Male")

#########################
#Load Status Survey Data#
#########################
survey<-read.csv("G:/My Drive/Research/Labs/COMPSYN/gendered ageism/data/gendered_ageism_img/status_survey.csv")
survey_ladder<-subset(survey, construct == "ladder")
survey_prestige<-subset(survey, construct == "prestige")
survey_status<-subset(survey, construct == "status")

######Aggregate Data
survey_ladder$measurement<-as.numeric(survey_ladder$measurement)
survey_ladder_agg<-survey_ladder %>% group_by(Social.Category) %>% 
  dplyr::summarise(avg_ladder = mean(measurement, na.rm=T), 
                   num_obvs = length(unique(ProlificID)), 
                   stdv_ladder = sd(measurement, na.rm=T), 
                   var_adder = var(measurement, na.rm=T))

survey_prestige$measurement<-as.factor(survey_prestige$measurement)
levels(survey_prestige$measurement)<-c(0, 1, -1, 2, -2)
survey_prestige$measurement<-as.numeric(as.character(survey_prestige$measurement))
survey_prestige_agg<-survey_prestige %>% group_by(Social.Category) %>% 
  dplyr::summarise(avg_prestige = mean(measurement, na.rm=T), 
                   stdv_prestige = sd(measurement, na.rm=T), 
                   var_prestige = var(measurement, na.rm=T))

survey_status$measurement<-as.factor(survey_status$measurement)
levels(survey_status$measurement)<-c(-1, 0, 1, -2, 2)
survey_status$measurement<-as.numeric(as.character(survey_status$measurement))
survey_status_agg<-survey_status %>% group_by(Social.Category) %>% 
  dplyr::summarise(avg_status = mean(measurement, na.rm=T), 
                   stdv_status = sd(measurement, na.rm=T), 
                   var_status = var(measurement, na.rm=T))

all_survey<-merge(survey_ladder_agg, survey_prestige_agg, by=c("Social.Category"))
all_survey<-merge(all_survey, survey_status_agg, by=c("Social.Category"))

########################
#Extended Data Figure 4#
########################

#Panel A#
survey_composite<-rbind(survey_status, survey_prestige)
survey_composite_agg<-survey_composite %>% group_by(ProlificID, Social.Category) %>% 
  dplyr::summarise(composite = mean(measurement, na.rm=T))
composite_full<-merge(survey_composite_agg, data_google_agg_bygen_clean_gap, by=c("Social.Category"))
composite_full$male_older_binary<-as.numeric(composite_full$male_older)

composite_full$comp_bin<-ntile(composite_full$composite, 6)
composite_full_plot<-composite_full %>% group_by(comp_bin) %>% 
  dplyr::summarise(pMaleOlder = prop.test(sum(male_older),length(male_older))$estimate, 
                   cilow = prop.test(sum(male_older),length(male_older))$conf.int[1],
                   cihi = prop.test(sum(male_older),length(male_older))$conf.int[2])

ggplot(composite_full_plot, aes(x = comp_bin, y = pMaleOlder, ymin = cilow, ymax = cihi, group=1)) + theme_bw() +
  geom_point(size = 10) + geom_errorbar(width=0.2) + geom_line(linewidth=0.5) + 
  xlab("Perceived Status of Occupation") + ylab("P(Men Older in Google Images)") + theme(
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
  scale_x_continuous(breaks = c(1:6)) + 
  scale_y_continuous(labels = scales::percent, breaks = c(0.75, 0.8, 0.85))

#Table S10#
mod_composite_full<-lm(male_older_binary ~ composite + ProlificID, data = composite_full)
summary(mod_composite_full)
mod_composite_full_clust_se <- vcovCL(mod_composite_full, cluster = ~ Social.Category)
coeftest(mod_composite_full, vcov = mod_composite_full_clust_se)

#Panel C (OPR Ratings: https://occupational-prestige.github.io/opratings/index.html)
OPR<-read.csv("G:/My Drive/Research/Labs/COMPSYN/gendered ageism/data/gendered_ageism_img/OPR_prestige_ratings.csv")
OPR_full<-merge(data_google_agg_bygen_clean_gap, OPR, by=c("Social.Category"))

#Panel B#
OPR_full$male_older_binary<-as.numeric(OPR_full$male_older)
cor.test(OPR_full$male_older_binary, OPR_full$OPR.Job.Rating)
cor.test(OPR_full$male_older_binary, OPR_full$GSS.Ratings.2012)

############
OPR_full$score_bins<-ntile(OPR_full$OPR.Job.Rating, 4)

OPR_full_agg<-OPR_full %>% group_by(score_bins) %>% 
  dplyr::summarise(pMaleOlder = prop.test(sum(male_older),length(male_older))$estimate, 
                   cilow = prop.test(sum(male_older),length(male_older))$conf.int[1],
                   cihi = prop.test(sum(male_older),length(male_older))$conf.int[2])

ggplot(OPR_full_agg, aes(x = score_bins, y = pMaleOlder, ymin = cilow, ymax = cihi, group=1)) + theme_bw() +
  geom_point(size = 10) + geom_errorbar(width=0.2) + geom_line(linewidth=0.5) + 
  xlab("Occupational Prestige Ratings (2022)") + 
  ylab("P(Men Older in Google Images)") + theme(
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
  coord_cartesian(ylim=c(0.63, 0.9)) + 
  scale_y_continuous(labels = scales::percent)


#######################
#Panel C (by earnings)#
#######################
census_earnings_dt<-read.csv("G:/My Drive/Research/Labs/COMPSYN/gendered ageism/data/gendered_ageism_img/census_occupation_income_data.csv")
data_google_agg_bygen_clean_gap$male_older_binary<-as.numeric(data_google_agg_bygen_clean_gap$male_older)
income_compare<-merge(data_google_agg_bygen_clean_gap, census_earnings_dt, by=c("Social.Category"))
income_compare$pay_gap_raw<-income_compare$Avg.Income.M - income_compare$Avg.Income.F
income_compare$log_pay_gap_raw<-log(income_compare$Avg.Income.M) - log(income_compare$Avg.Income.F)
income_compare$pay_gap_abs<-abs(income_compare$pay_gap_raw)
income_compare$log_income<-log(income_compare$Avg.Income)
income_compare$log_income_error<-log(income_compare$Avg.Income.Err)
income_compare_comp<-income_compare[complete.cases(income_compare),]

income_compare_comp$income_bin <- cut(income_compare_comp$log_income, 
                                      breaks = quantile(income_compare_comp$log_income, 
                                                        probs = seq(0, 1, 0.25)), 
                                      include.lowest = TRUE)

income_compare_agg<-income_compare_comp %>% group_by(income_bin) %>% 
  dplyr::summarise(pmale_older = sum(male_older)/length(male_older), 
                   cilow = prop.test(sum(male_older), length(male_older))$conf.int[1],
                   cihi = prop.test(sum(male_older), length(male_older))$conf.int[2])

ggplot(income_compare_agg,  aes(x = income_bin, y = pmale_older, ymin = cilow, ymax = cihi, group=1)) + theme_bw() +
  geom_point(size = 10) + geom_errorbar(width = 0.2) + 
  geom_line(linewidth = 0.5) + 
  xlab("Logged Occupation Earnings (Quartiles)") + ylab("P(Men Older in Google Images)") + theme(
    axis.title.x=element_text(size=35),
    axis.title.y=element_text(size=35),
    plot.title=element_text(size=35, hjust=0.5),
    legend.text=element_text(size=35),
    legend.title=element_blank(),
    legend.position="none",
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black",fill="white", linewidth=1.4),
    axis.text.x=element_text(size = 30, angle = 0),
    axis.text.y=element_text(size = 30),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  scale_y_continuous(labels = scales::percent)

#Table S12#
cor.test(income_compare$log_income, income_compare$male_older_binary)
income_compare_comp<-income_compare[complete.cases(income_compare),]
cor.test(income_compare_comp$log_income, income_compare_comp$male_older_binary)

table_s12<-lm(male_older_binary ~ log_income + log_income_error + year, data=income_compare)
summary(table_s12)

table_s12_clust_se <- vcovCL(table_s12, cluster = ~ Social.Category)
coeftest(table_s12, vcov = table_s12_clust_se)

######################
#Panel D (by pay gap)#
######################
cor.test(subset(income_compare, pay_gap_raw> 0)$raw_age_gap, 
         subset(income_compare, pay_gap_raw> 0)$pay_gap_raw, method="spearman")

income_compare_agg<-income_compare %>% group_by(Occupation, year) %>% 
  dplyr::summarise(pay_gap_raw = mean(pay_gap_raw), 
                   raw_age_gap = mean(raw_age_gap))

###
cor.test(income_compare_comp$raw_age_gap, income_compare_comp$pay_gap_raw, paired=T)
cor.test(income_compare$raw_age_gap, income_compare$pay_gap_raw, paired=T)

income_compare_comp$pay_gap_bin <- cut(income_compare_comp$pay_gap_raw, 
                                       breaks = quantile(income_compare_comp$pay_gap_raw, 
                                                         probs = seq(0, 1, 0.25)), 
                                       include.lowest = TRUE, 
                                       dig.lab=1)

income_compare_agg<-income_compare_comp %>% group_by(pay_gap_bin) %>% 
  dplyr::summarise(cilow = t.test(raw_age_gap)$conf.int[1],
                   cihi = t.test(raw_age_gap)$conf.int[2],
                   raw_age_gap= mean(raw_age_gap), 
                   pay_gap_raw = mean(pay_gap_raw),
                   pmale_older = sum(male_older)/length(male_older))

ggplot(income_compare_agg,  aes(x = pay_gap_bin, y = raw_age_gap, ymin = cilow, ymax = cihi, group=1)) + theme_bw() +
  geom_point(size = 10) + geom_errorbar(width = 0.2) + 
  geom_line(linewidth = 0.5) + 
  xlab("Pay Gap by Occupation (Quartiles)\nMale - Female Earnings") + ylab("Age Gap in Google Images\nMale - Female Age") + theme(
    axis.title.x=element_text(size=35),
    axis.title.y=element_text(size=35),
    plot.title=element_text(size=35, hjust=0.5),
    legend.text=element_text(size=35),
    legend.title=element_blank(),
    legend.position="none",
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black",fill="white", linewidth=1.4),
    axis.text.x=element_text(size = 26, angle = 0),
    axis.text.y=element_text(size = 30),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    panel.background = element_blank(), axis.line = element_line(colour = "black")) 

############
#Figure S12#
############
cor.test(all_survey$avg_ladder, all_survey$avg_prestige, paired=T)
cor.test(all_survey$avg_ladder, all_survey$avg_status, paired=T)
cor.test(all_survey$avg_prestige, all_survey$avg_status, paired=T)

all_survey$composite<-sapply(1:nrow(all_survey), function(x) mean(c(all_survey[x,]$avg_status, all_survey[x,]$avg_prestige)))

figS12<-data.frame(Status = all_survey$avg_status, 
                     Prestige = all_survey$avg_prestige, 
                     Composite = all_survey$composite, 
                     GSS.Ladder = all_survey$avg_ladder)

figS12_mat <- round(cor(figS12),2)

get_lower_tri<-function(figS12_mat){
  figS12_mat[upper.tri(figS12_mat)] <- NA
  return(figS12_mat)
}

get_upper_tri <- function(figS12_mat){
  figS12_mat[lower.tri(figS12_mat)]<- NA
  return(figS12_mat)
}

upper_tri <- get_upper_tri(figS12_mat)
melted_figS12_mat <- melt(upper_tri, na.rm = TRUE)

ggplot(melted_figS12_mat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+ theme_bw() + coord_fixed() + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme(axis.text.x = element_text(angle = 0, size = 30), 
        axis.text.y = element_text(size = 30, angle = 90))+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 20) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.5, 0.8),
    legend.direction = "horizontal",
    legend.text = element_text(size=30),
    legend.title = element_text(size=30))+
  guides(fill = guide_colorbar(barwidth = 20, barheight = 2, title.position = "top", title.hjust = 0.5))

###########
#Table S11#
###########
ladder_full<-merge(dt_ladder, data_google_agg_bygen_clean_gap, by=c("Social.Category"))
ladder_full$male_older_binary<-as.numeric(ladder_full$male_older)

cor.test(ladder_full$male_older_binary, ladder_full$measurement)

mod_ladder_full<-lm(male_older_binary ~ measurement + ProlificID, data = ladder_full)
summary(mod_ladder_full)
mod_ladder_full_clust_se <- vcovCL(mod_ladder_full, cluster = ~ Social.Category)
coeftest(mod_ladder_full, vcov = mod_ladder_full_clust_se)

