###############
#Load packages#
###############
rm(list=ls());gc()
library(dplyr);library(ggplot2);library(tidyverse);library(tidyr)
library(multiwayvcov);library(lmtest); library(emmeans); library(svglite)
library(broom); library(gtsummary); library(flextable); 

datapath<-"" #set path to where data is saved
savepath<-"" #set path to where you want to save the figures 

#Functions
min_max_norm<-function(x){(x - min(x,na.rm=T))/(max(x,na.rm=T) - min(x,na.rm=T))}

#Load data
control<-read.csv(paste(datapath, "experiment_control.csv", sep=""))
control$gender_cond<-"control"
control_agg<-control %>% group_by(category) %>% dplyr::summarise(age=mean(age), ideal_age=mean(ideal_age))
colnames(control_agg)<-c("category","c_age", "c_ideal")

treatment<-read.csv(paste(datapath, "experiment_treatment.csv", sep=""))
treatment$gender_cond<-treatment$gender
treatment$ideal_age<-"NA"
treatment_agg<-treatment %>% group_by(category, gender) %>% dplyr::summarise(age=mean(age))

treatment_agg_full<-merge(treatment_agg, control_agg, by=c("category"))
treatment_agg_full_main<-subset(treatment_agg_full, gender != "Not Sure")

treatment_agg_full_main$age_cent<-treatment_agg_full_main$age-treatment_agg_full_main$c_age
treatment_agg_full_main$age_ideal_cent<-treatment_agg_full_main$age-treatment_agg_full_main$c_age

t.test(subset(treatment_agg_full_main, gender == "Female")$age, 
       subset(treatment_agg_full_main, gender == "Male")$age, paired=T)

treatment_full<-merge(treatment, control_agg, by=c("category"))
treatment_full_main<-subset(treatment_full, gender != "Not Sure")

treatment_full_main$age_cent<-treatment_full_main$age-treatment_full_main$c_age
treatment_full_main$age_ideal_cent<-treatment_full_main$age-treatment_full_main$c_ideal

control$gender_num<-as.factor(control$gender)
levels(control$gender_num)<-c(-1,1,0)
control$gender_num<-as.numeric(as.character(control$gender_num))

treatment$gender_num<-as.factor(treatment$gender)
levels(treatment$gender_num)<-c(-1,1,0)
treatment$gender_num<-as.numeric(as.character(treatment$gender_num))

dt_main<-rbind(treatment, control)

######################
#####MAIN RESULTS#####
######################

#########
#Fig. 3A#
#########
ggplot(treatment_full_main, aes(x = age_cent, fill=gender)) +
  geom_density(alpha=0.6, size=1.2) + theme_bw() + 
  scale_fill_manual(values=c("orange", "dodgerblue")) + 
  ylab("Density") + xlab("Estimated Age Relative to Control") + 
  theme(legend.text=element_text(size=40),
        legend.position=c(0.735, 0.85),
        legend.title = element_text(size = 40),
        plot.title=element_text(size = 40, hjust = 0.5),
        axis.title.y=element_text(size = 40, hjust = 0.5),
        axis.title.x=element_text(size = 40, hjust = 0.5),
        axis.text.x=element_text(size = 40, hjust = 0.6),
        axis.text.y=element_text(size = 40, hjust = 0.6),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +   
  scale_x_continuous(limits = c(-22, 30)) + 
  geom_vline(xintercept = 0, linetype="dotted", linewidth=2) + 
  geom_vline(xintercept = mean(subset(treatment_full_main, gender=="Female")$age_cent, na.rm=T), 
             linetype="solid", linewidth=2, color="orange") + 
  geom_vline(xintercept = mean(subset(treatment_full_main, gender=="Male")$age_cent, na.rm=T), 
             linetype="solid", linewidth=2, color="dodgerblue") + 
  guides(fill = guide_legend(title = "Image Uploaded"))
ggsave('fig3A.svg', width=11, height=11, path = savepath)

t.test(subset(treatment_full_main, gender == "Female")$age_cent)
t.test(subset(treatment_full_main, gender == "Male")$age_cent)
t.test(subset(treatment_full_main, gender == "Female")$age, subset(treatment_full_main, gender == "Male")$age)
t.test(subset(treatment_full_main, gender == "Female")$age, subset(treatment_full_main, gender == "Male")$age)
t.test(subset(treatment_full_main, gender == "Female")$age_cent, subset(treatment_full_main, gender == "Male")$age_cent)
t.test(subset(treatment_full_main, gender == "Female")$age_ideal_cent, subset(treatment_full_main, gender == "Male")$age_ideal_cent)

########
#FIG 3B#
########
dt_main$gender<-as.factor(dt_main$gender)
dt_main$gender <- relevel(dt_main$gender, ref = 3)

dt_main_g<-subset(dt_main, gender %in% c("Male", "Female"))
dt_main_g$gender<-as.factor(dt_main_g$gender)
dt_main_g$gender <- relevel(dt_main_g$gender, ref = 3)

dt_mod<-lm(age ~ condition * gender + category + subj, data = dt_main_g)
summary(dt_mod)

t.test(subset(dt_main_g, condition=="Image" & gender=="Female")$age, 
       subset(dt_main_g, condition!="Image" & gender=="Female")$age)

###########
#visualize#
###########
emm_interaction <- emmeans(dt_mod, specs = pairwise ~ condition * gender, rg.limit = 98065)
emm_interaction_df <- data.frame(emm_interaction$emmeans)
emm_interaction_df$gender<-factor(emm_interaction_df$gender, levels=c("Male", "Female"))

ggplot(emm_interaction_df, aes(x = condition, y = emmean, color = gender, group = gender)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, size=3) +
  scale_color_manual(values=c("dodgerblue", "orange"))+ 
  ggtitle("Partial Effect Plot\n(w. Occupation and Subj. FEs)") + 
  geom_line(size=3) +
  geom_point(size = 10) +
  theme_bw() +
  labs(y = "Estimated Age", x = "Condition", color = "Gender") + 
  theme(axis.text.x = element_text(size=40), 
        axis.text.y = element_text(size=40),
        axis.title.x = element_text(size=40),
        axis.title.y = element_text(size=40),
        legend.position = c(0.8,0.5), 
        legend.text=element_text(size=30),
        legend.title=element_blank(),
        axis.line = element_line(colour = "black"), 
        legend.box.background = element_rect(colour = "black"),
        legend.background = element_blank(), 
        plot.title = element_text(size=40, hjust=0.5))
ggsave('fig3B.svg', width=11, height=12.2, path = savepath)

#########
#FIG. 3C#
#########

#Load other experimental data (from Guilbeault et al. 2024 in Nature, "Online Images Amplify Gender Bias")
nature_exp<-read.csv(paste(datapath, "experiment_Guilbeaultetal2024.csv", sep=""))
nature_exp_cntrl<-subset(nature_exp, condition == "Control")
nature_exp_cntrl_agg<-nature_exp_cntrl %>% group_by(category) %>% 
  dplyr::summarise(gender.rate=mean(gender.rate), str_stereo = mean(str_stereo))

nature_exp_img<-subset(nature_exp, condition == "Image")
nature_exp_img_agg<-nature_exp_img %>% group_by(category) %>% 
  dplyr::summarise(n.gender.rate=mean(gender.rate), n.str_stereo = mean(str_stereo), n.gender_assoc = mean(gender_num, na.rm=T))

control_agg<- control %>% group_by(category) %>% dplyr::summarise(cntrl_gender_assoc = mean(gender_num), age=mean(age), ideal_age=mean(ideal_age))

control_m<-merge(nature_exp_cntrl_agg, control_agg, by=c("category"))

treatment_agg<-treatment %>% group_by(category) %>% 
  dplyr::summarise(age=mean(age), gender_assoc = mean(gender_num))

treatment_agg_m<-merge(nature_exp_img_agg, treatment_agg, by=c("category"))
treatment_agg_m2<-merge(treatment_agg_m, control_m, by=c("category"))

hireability_comp_fig<-
  rbind(
    data.frame(category=control_m$category, gender = control_m$gender.rate, 
               ideal_age=control_m$ideal_age, measure="Ratings.Exp2.Control"), 
    data.frame(category=control_m$category, gender = control_m$cntrl_gender_assoc, 
               ideal_age=control_m$ideal_age, measure="Ratings.Exp1.Control"), 
    data.frame(category=treatment_agg_m2$category, gender = treatment_agg_m2$n.gender.rate, 
               ideal_age=treatment_agg_m2$ideal_age, measure="Ratings.Exp2.Image"), 
    data.frame(category=treatment_agg_m2$category, gender = treatment_agg_m2$n.gender_assoc, 
               ideal_age=treatment_agg_m2$ideal_age, measure="Uploads.Exp2.Image"), 
    data.frame(category=treatment_agg_m2$category, gender = treatment_agg_m2$gender_assoc, 
               ideal_age=treatment_agg_m2$ideal_age, measure="Uploads.Exp1.Image")
  )

hireability_comp_fig_main<-subset(hireability_comp_fig, measure %in% c("Uploads.Exp1.Image", "Ratings.Exp1.Control"))
hireability_comp_fig_main$measure<-as.factor(hireability_comp_fig_main$measure)
levels(hireability_comp_fig_main$measure)<-c("Gender Ratings (Control Condition)", "Image Uploads (Image Condition)")

ggplot(hireability_comp_fig_main, aes(x=gender, y=ideal_age, color=measure, fill=measure, linetype=measure))+
  geom_point(size=8, alpha=0.6) + 
  geom_smooth(linewidth=3, method='lm', formula= y~x, se=T, alpha=0.3) + 
  scale_color_manual(values = c("#CC79A7", "#009E73")) + 
  xlab("Gender Associations") + 
  ylab("Perceived Ideal Hiring Age") + theme_bw() + 
  theme(axis.text.x = element_text(size=40), 
        axis.text.y = element_text(size=40),
        axis.title.x = element_text(size=40),
        axis.title.y = element_text(size=40),
        legend.position = "top", 
        legend.text=element_text(size=40),
        legend.title=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        legend.box.background = element_rect(colour = "black"),
        legend.background = element_blank()) + 
  geom_vline(xintercept = 0, linetype="dotted", size=2) + 
  scale_y_continuous(breaks=c(20,25,30,35,40,45)) + 
  scale_x_continuous(limits = c(-1.05,1.05)) + 
  guides(color = guide_legend(nrow = 3, byrow = TRUE), fill = guide_legend(nrow = 3, byrow = TRUE))
ggsave('fig3C.svg', width=11, height=12.75, path = savepath)

cor.test(subset(hireability_comp_fig, measure=="Ratings.Exp1.Control")$gender, 
         subset(hireability_comp_fig, measure=="Ratings.Exp1.Control")$ideal_age)
cor.test(subset(hireability_comp_fig, measure=="Ratings.Exp2.Control")$gender, 
         subset(hireability_comp_fig, measure=="Ratings.Exp2.Control")$ideal_age)
cor.test(subset(hireability_comp_fig, measure=="Ratings.Exp2.Image")$gender, 
         subset(hireability_comp_fig, measure=="Ratings.Exp2.Image")$ideal_age)
cor.test(subset(hireability_comp_fig, measure=="Uploads.Exp1.Image")$gender, 
         subset(hireability_comp_fig, measure=="Uploads.Exp1.Image")$ideal_age)
cor.test(subset(hireability_comp_fig, measure=="Uploads.Exp2.Image")$gender, 
         subset(hireability_comp_fig, measure=="Uploads.Exp2.Image")$ideal_age)

###############################
###############################
#####SUPPLEMENTARY RESULTS#####
###############################
###############################
colnames(dt_main)[1]<-"Participant.id"

#Load subject demographics 
demo<-read.csv(paste(datapath, "experiment_demo.csv", sep=""))
dt_full<-merge(dt_main, demo, by=c("Participant.id"))
dt_full$Subj.Age<-as.numeric(dt_full$Subj.Age)

mean(dt_full$Subj.Age)
table(dt_full$Subj.Gender)/sum(table(dt_full$Subj.Gender))

dt_full %>% group_by(Subj.Gender) %>% dplyr::summarise(Avg.Age = mean(Subj.Age))
t.test(subset(dt_full, Subj.Gender=="Female")$Subj.Age, subset(dt_full, Subj.Gender=="Male")$Subj.Age)

#Models# 
dt_full$gender_cond<-as.character(dt_full$gender_cond)
dt_demo_comp<-subset(dt_full, !gender_cond %in% c("Not Sure"))

tableS19<-lm(age ~ gender_cond + category + Subj.Age + Subj.Gender, data = dt_demo_comp)
summary(mod)

tableS19_tbl <- tbl_regression(tableS19, pvalue_fun = ~format(.x, scientific = TRUE)) %>% 
  modify_header(label = "**Variables**") %>% 
  add_glance_table(
    include = c(statistic, r.squared, adj.r.squared, nobs, df, AIC, df.residual, sigma) #everything()
  )

as_flex_table(tableS19_tbl) %>% flextable::save_as_docx(path = paste(savepath, "tableS19.docx", sep=""))

dt_demo_comp_img<-subset(dt_demo_comp, gender_cond != "control")
dt_demo_comp_img$demo_match_img<-dt_demo_comp_img$Subj.Gender == dt_demo_comp_img$gender
dt_demo_comp_img$Age.Diff<-abs(dt_demo_comp_img$Subj.Age - dt_demo_comp_img$age)

tableS20<-lm(age ~ gender_cond + category + demo_match_img + Age.Diff, data = dt_demo_comp_img)
summary(tableS20)

tableS20_tbl <- tbl_regression(tableS20, pvalue_fun = ~format(.x, scientific = TRUE)) %>% 
  modify_header(label = "**Variables**") %>% 
  add_glance_table(
    include = c(statistic, r.squared, adj.r.squared, nobs, df, AIC, df.residual, sigma) #everything()
  )

as_flex_table(tableS20_tbl) %>% flextable::save_as_docx(path = paste(savepath, "tableS20.docx", sep=""))

##########
#FIG. S17#
##########
ggplot(hireability_comp_fig, aes(x=gender, y=ideal_age, color=measure, fill=measure))+
  geom_point(size=8, alpha=0.4) + 
  geom_smooth(linewidth=3, method='lm', formula= y~x, se=F) + 
  xlab("Gender Associations") + 
  ylab("Perceived Ideal Hiring Age") + theme_bw() + 
  theme(axis.text.x = element_text(size=40), 
        axis.text.y = element_text(size=40),
        axis.title.x = element_text(size=40),
        axis.title.y = element_text(size=40),
        legend.position = "top", 
        legend.text=element_text(size=30),
        legend.title=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        legend.box.background = element_rect(colour = "black"),
        legend.background = element_blank()) + 
  geom_vline(xintercept = 0, linetype="dotted", size=2) + 
  scale_y_continuous(breaks=c(20,25,30,35,40,45)) + 
  scale_x_continuous(limits = c(-1.05,1.05)) + 
  guides(color = guide_legend(nrow = 3, byrow = TRUE), fill = guide_legend(nrow = 3, byrow = TRUE))

##########
#FIG. S18#
##########
hire_likert<-read.csv(paste(datapath, "hire_likert.csv", sep=""))
treatment_likert<-merge(treatment, hire_likert, by=c("subj", "category"))
treatment_likert<-subset(treatment_likert, gender != "Not Sure")
treatment_likert<-treatment_likert %>% group_by(category) %>% dplyr::mutate(age_norm=min_max_norm(age))

cor.test(subset(treatment_likert, gender=="Male")$age, subset(treatment_likert, gender=="Male")$hire_num, method="spearman")
cor.test(subset(treatment_likert, gender=="Female")$age, subset(treatment_likert, gender=="Female")$hire_num, method="spearman")
cor.test(subset(treatment_likert, gender=="Male")$age_norm, subset(treatment_likert, gender=="Male")$hire_num)
cor.test(subset(treatment_likert, gender=="Female")$age_norm, subset(treatment_likert, gender=="Female")$hire_num)

ggplot(treatment_likert, aes(x = age_norm, y = hire_num, color =  gender))+ theme_bw() + 
  geom_smooth(linewidth=2, se=T, method="lm") + 
  xlab("Age (Normalized by Occupation)") + 
  ylab("Hireability of Person Depicted") + 
  scale_color_manual(values=c("orange", "dodgerblue")) + 
  theme(axis.text.x = element_text(size=40), 
        axis.text.y = element_text(size=40),
        axis.title.y = element_text(size=40),
        axis.title.x = element_text(size=40),
        plot.title = element_text(size=40, hjust=0.5),
        legend.position = c(0.8,0.8), legend.text=element_text(size=40),
        legend.title=element_blank(),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

###########################
###########################
#Merge/Compare with Census#
###########################
###########################
census_map<-read.csv(paste(datapath, "census_map.csv", sep=""))
census_map_full<-merge(control, census_map, by=c("category"))

census_map_full_long<-rbind(
  data.frame(category=census_map_full$category, subj = census_map_full$subj, condition = census_map_full$condition, census.match=census_map_full$census.match, gender = census_map_full$gender, age = census_map_full$age, ideal_age = census_map_full$ideal_age, Census_age=census_map_full$Age_2020, Census_year = 2020), 
  data.frame(category=census_map_full$category, subj = census_map_full$subj, condition = census_map_full$condition, census.match=census_map_full$census.match, gender = census_map_full$gender, age = census_map_full$age, ideal_age = census_map_full$ideal_age, Census_age=census_map_full$Age_2021, Census_year = 2021), 
  data.frame(category=census_map_full$category,  subj = census_map_full$subj,condition = census_map_full$condition, census.match=census_map_full$census.match, gender = census_map_full$gender, age = census_map_full$age, ideal_age = census_map_full$ideal_age, Census_age=census_map_full$Age_2022, Census_year = 2022), 
  data.frame(category=census_map_full$category,  subj = census_map_full$subj,condition = census_map_full$condition, census.match=census_map_full$census.match, gender = census_map_full$gender,  age = census_map_full$age, ideal_age = census_map_full$ideal_age, Census_age=census_map_full$Age_2023, Census_year = 2023)
)

census_map_full_long$ideal_age<-as.numeric(as.character(census_map_full_long$ideal_age))
census_map_full_long$Census_year<-as.factor(census_map_full_long$Census_year)
census_map_full_long<-unique(census_map_full_long)
census_map_full_long$subj<-as.factor(census_map_full$subj)

##########
#Combined#
##########
cor.test(census_map_full_long$age, census_map_full_long$Census_age)

tableS21<-lm(Census_age ~ age + Census_year + subj, data = census_map_full_long)
summary(tableS21)

tableS21_tbl <- tbl_regression(tableS21, pvalue_fun = ~format(.x, scientific = TRUE)) %>% 
  modify_header(label = "**Variables**") %>% 
  add_glance_table(
    include = c(statistic, r.squared, adj.r.squared, nobs, df, AIC, df.residual, sigma) #everything()
  ) 

as_flex_table(tableS21_tbl) %>% flextable::save_as_docx(path = paste(savepath, "tableS21.docx", sep=""))

#ideal hiring age
cor.test(census_map_full_long$ideal_age, census_map_full_long$Census_age)

tableS22<-lm(Census_age ~ ideal_age + Census_year + subj, data = census_map_full_long)
summary(tableS22)

tableS22_tbl <- tbl_regression(tableS22, pvalue_fun = ~format(.x, scientific = TRUE)) %>% 
  modify_header(label = "**Variables**") %>% 
  add_glance_table(
    include = c(statistic, r.squared, adj.r.squared, nobs, df, AIC, df.residual, sigma) #everything()
  ) 

as_flex_table(tableS22_tbl) %>% flextable::save_as_docx(path = paste(savepath, "tableS22.docx", sep=""))

###########
#Table S23#
###########
dt_main_binary<-subset(dt_main, gender != "Not Sure")
dt_main_binary$age<-as.numeric(dt_main_binary$age)
dt_main_binary$ideal_age<-as.numeric(dt_main_binary$ideal_age)

img_ext<-subset(dt_main_binary, condition=="Image")
cntrol_ext<-subset(dt_main_binary, condition!="Image")

cntrol_ext_agg<-cntrol_ext %>% group_by(category) %>% 
  dplyr::summarise(ideal_age = mean(ideal_age, na.rm=T), 
                   gender_mean = mean(gender_num))

cntrol_ext_agg$male_category<-cntrol_ext_agg$gender_mean>0

img_ext_full<-merge(img_ext[,!colnames(img_ext) %in% c("ideal_age")], cntrol_ext_agg, by=c("category"))
img_ext_full$age_ideal_gap<-abs(img_ext_full$age - img_ext_full$ideal_age)

table_S23<-lm(age_ideal_gap ~ gender + male_category + subj + category, data = img_ext_full)
summary(table_S23)

table_S23_tbl <- tbl_regression(table_S23, pvalue_fun = ~format(.x, scientific = TRUE), 
                              include=c("gender", "male_category")) %>% 
  modify_header(label = "**Variables**") %>% 
  add_glance_table(
    include = c(statistic, r.squared, adj.r.squared, nobs, df, AIC, df.residual, sigma) #everything()
  ) 


