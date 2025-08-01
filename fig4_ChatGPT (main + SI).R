###############
#Load packages#
###############
rm(list=ls());gc();
library(dplyr); library(ggplot2); library(tidyverse);
library(lubridate); library(sjPlot); library(sjmisc)
library(lmtest); library(sandwich); library(gtsummary)
library(flextable); library(svglite)


datapath<-"" #set path to where data is saved
savepath<-"" #set path to where you want to save the figures 
windows()

#########
#FIG. 4A#
#########
dt<-read.csv(paste(datapath, "resumes_main.csv", sep=""))
dt$bday<-mdy(dt$Date.of.Birth)
dt$gradday<-mdy(dt$Graduation.Date)
dt$age<-as.numeric((today()-dt$bday)/365)
dt$age_since_grad<-as.numeric((today()-dt$gradday)/365)

cor.test(dt$age, dt$age_since_grad)
cor.test(dt$age, dt$Total.Experience)
cor.test(dt$age_since_grad, dt$Total.Experience)

####################
dt_clean<-dt %>% group_by(Condition, Name, Occupation, Gender) %>% slice_sample(n=50)
dt_clean %>% group_by(Gender) %>% dplyr::summarise(age = mean(age, na.rm=T), 
                                                   age_since_grad = mean(age_since_grad, na.rm=T), 
                                                   Number.of.Skills = mean(Number.of.Skills, na.rm=T), 
                                                   Total.Experience = mean(Total.Experience, na.rm=T))

t.test(subset(dt_clean, Gender == "Female")$age, subset(dt_clean, Gender == "Male")$age)
t.test(subset(dt_clean, Gender == "Female")$age_since_grad, subset(dt_clean, Gender == "Male")$age_since_grad)
t.test(subset(dt_clean, Gender == "Female")$Number.of.Skills, subset(dt_clean, Gender == "Male")$Number.of.Skills)
t.test(subset(dt_clean, Gender == "Female")$Total.Experience, subset(dt_clean, Gender == "Male")$Total.Experience)

dt_clean_simp<-subset(dt_clean, Condition != "Control V1 - Anonymous + Gender")
dt_clean_simp$Gender<-as.factor(dt_clean_simp$Gender)
levels(dt_clean_simp$Gender)<-c("Female", "Male", "Control")

#With Control
dt_clean_simp$Gender<-factor(dt_clean_simp$Gender, levels=c("Control", "Female", "Male"))

mod_age<-lm(age ~ Gender + Occupation + Name, data = dt_clean_simp)
summary(mod_age)

mod_exp<-lm(Total.Experience ~ Gender + Occupation + Name, data = dt_clean_simp)
summary(mod_exp)

mod_grad<-lm(age_since_grad ~ Gender + Occupation + Name, data = dt_clean_simp)
summary(mod_grad)

#Just Treatment
dt_treat<-subset(dt_clean_simp, Condition != "Control V1 - Anonymous")
mod_t_age<-lm(age ~ Gender + Occupation + Name, data = dt_treat)
mod_t_age_sum<-summary(mod_t_age)
mod_t_age_df<-as.data.frame(mod_t_age_sum$coefficients)
mod_t_age_df$variable<-row.names(mod_t_age_df)
mod_t_age_df$y<-"Applicant\nAge"

mod_t_exp<-lm(Total.Experience ~ Gender + Occupation + Name, data = dt_treat)
mod_t_exp_sum<-summary(mod_t_exp)
mod_t_exp_df<-as.data.frame(mod_t_exp_sum$coefficients)
mod_t_exp_df$variable<-row.names(mod_t_exp_df)
mod_t_exp_df$y<-"Years of\nexperience"

mod_t_grad<-lm(age_since_grad ~ Gender + Occupation + Name, data = dt_treat)
mod_t_grad_sum<-summary(mod_t_grad)
mod_t_grad_df<-as.data.frame(mod_t_grad_sum$coefficients)
mod_t_grad_df$variable<-row.names(mod_t_grad_df)
mod_t_grad_df$y<-"Years since\ngraduation"

plot_main<-rbind(
  subset(mod_t_age_df, variable == "GenderMale"), 
  subset(mod_t_exp_df, variable == "GenderMale"), 
  subset(mod_t_grad_df, variable == "GenderMale")
)

plot_main$xmin<-plot_main$Estimate - plot_main$`Std. Error`
plot_main$xmax<-plot_main$Estimate + plot_main$`Std. Error`

plot_main$y<-factor(plot_main$y, levels=rev(c("Applicant\nAge", "Years since\ngraduation", "Years of\nexperience")))

ggplot(plot_main, aes(x = Estimate, y = y, xmin=xmin, xmax = xmax)) + theme_bw() +
  geom_point(size=15) + geom_errorbar(width=0.2, linewidth=2) + 
  xlab("Effect of Male Name on Resume Feature") + 
  theme(
    axis.title.x=element_text(size = 50),
    axis.title.y=element_blank(),
    plot.title=element_blank(),
    legend.text=element_text(size=50),
    legend.title=element_blank(),
    legend.position="top",
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black",fill="white", linewidth=1.4),
    axis.text.x=element_text(size = 50, vjust=0.8),axis.text.y=element_text(size = 50),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  geom_vline(xintercept=0, linetype="dotted", linewidth=2) + 
  coord_cartesian(xlim=c(-0.5, 1.8))
#ggsave('fig4A.svg', width=18, height=18, path = savepath)

###############################################
#Analyse effect of gender on control condition#
###############################################
dt_anon_wgender<-subset(dt, Condition == "Control V1 - Anonymous + Gender")
dt_anon_wgender$Resume_c<-tolower(dt_anon_wgender$Resume)
dt_anon_wgender$male_ext<-sapply(1:nrow(dt_anon_wgender), function(x) grepl(" male ", dt_anon_wgender[x,]$Resume_c, ignore.case = TRUE))
dt_anon_wgender$male_ext<-as.factor(dt_anon_wgender$male_ext)
levels(dt_anon_wgender$male_ext)<-c(NA,1)
dt_anon_wgender$male_ext<-as.numeric(as.character(dt_anon_wgender$male_ext))
dt_anon_wgender$female_ext<-sapply(1:nrow(dt_anon_wgender), function(x) grepl("female ", dt_anon_wgender[x,]$Resume_c, ignore.case = TRUE))
dt_anon_wgender$female_ext<-as.factor(dt_anon_wgender$female_ext)
levels(dt_anon_wgender$female_ext)<-c(NA,-1)
dt_anon_wgender$female_ext<-as.numeric(as.character(dt_anon_wgender$female_ext))
dt_anon_wgender$row<-1:nrow(dt_anon_wgender)
dt_anon_wgender<-dt_anon_wgender %>% group_by(row) %>% mutate(gender_ext = sum(c(male_ext, female_ext), na.rm=T))
dt_anon_wgender$gender_ext<-as.factor(dt_anon_wgender$gender_ext)
levels(dt_anon_wgender$gender_ext)<-c("Female", "NA","Male")

dt_anon_wgender %>% group_by(gender_ext) %>% 
  dplyr::summarise(age = mean(age, na.rm=T), 
                   Total.Experience = mean(Total.Experience, na.rm=T), 
                   age_since_grad = mean(age_since_grad, na.rm=T))

t.test(subset(dt_anon_wgender, gender_ext == "Female")$age, 
       subset(dt_anon_wgender, gender_ext == "Male")$age)

t.test(subset(dt_anon_wgender, gender_ext == "Female")$Total.Experience, 
       subset(dt_anon_wgender, gender_ext == "Male")$Total.Experience)

t.test(subset(dt_anon_wgender, gender_ext == "Female")$age_since_grad, 
       subset(dt_anon_wgender, gender_ext == "Male")$age_since_grad)

mod_cntrl_age<-lm(age ~ Occupation + gender_ext, data = dt_anon_wgender)
summary(mod_cntrl_age)

mod_cntrl_totalexp<-lm(Total.Experience ~ Occupation + gender_ext, data = dt_anon_wgender)
summary(mod_cntrl_totalexp)

mod_cntrl_grad<-lm(age_since_grad ~ Occupation + gender_ext, data = dt_anon_wgender)
summary(mod_cntrl_grad)

##########
#FIG. 4BC#
##########
scores_dt<-read.csv(paste(datapath, "resume_scores.csv", sep=""))
scores_dt$bday<-mdy(scores_dt$Date.of.Birth)
scores_dt$gradday<-mdy(scores_dt$Graduation.Date)
scores_dt$age<-as.numeric((today()-scores_dt$bday)/365)
scores_dt$age_since_grad<-as.numeric((today()-scores_dt$gradday)/365)
scores_dt<-scores_dt %>% group_by(Condition, Name, Occupation, Gender) %>% sample_n(size=20)

cor.test(scores_dt$age, scores_dt$score)
cor.test(scores_dt$Total.Experience, scores_dt$score)
cor.test(scores_dt$age_since_grad, scores_dt$score)

scores_dt_treatment<-subset(scores_dt, Condition=="Treatment")
cor.test(scores_dt_treatment$age, scores_dt_treatment$score)
cor.test(scores_dt$age, scores_dt$score)

#########
#Fig. 4B#
#########
ggplot(scores_dt_treatment, aes(x = age, y = score)) + theme_bw() +
  geom_point(color = "black", shape=21, stroke=2, alpha = 0.05, size=20) + 
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 5, color="red", data = scores_dt_treatment) + 
  xlab("Applicant Age") + ylab("Resume Score") + theme(
    axis.title.x=element_text(size=35),
    axis.title.y=element_text(size=35),
    plot.title=element_text(size=35, hjust=0.5), 
    legend.text=element_text(size=35),
    legend.title=element_blank(),
    legend.position="none",
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black",fill="white", linewidth=1.4),
    axis.text.x=element_text(size = 35, vjust=0.8),axis.text.y=element_text(size = 35),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  coord_cartesian(ylim=c(83,100))
#ggsave('fig4B.svg', width=9, height=9, path = savepath)

mod<-lm(score ~ Gender + age + Total.Experience + Name + Occupation, data = scores_dt)
summary(mod)

mod2<-lm(score ~ Gender + age + Total.Experience + Name + Occupation, data = subset(scores_dt, score<100))
summary(mod2)

mod3<-lm(score ~ age * Gender + Name + Occupation, data = subset(scores_dt, Condition=="Treatment"))
summary(mod3)

#########
#Fig. 4C#
#########
theme_set(theme_sjplot())

plot_model(mod3, type = "int", show.p = TRUE,wrap.title = 100,wrap.labels = 100, 
           line.size = 5, grid.breaks = FALSE, value.size=10) + theme_bw() + 
  xlab("Applicant Age") + ylab("Resume Score") + 
  scale_color_manual(values=c("orange", "dodgerblue")) + 
  theme(
    axis.title.x=element_text(size=35),
    axis.title.y=element_text(size=35),
    plot.title=element_blank(), 
    legend.text=element_text(size=35),
    legend.title=element_blank(),
    legend.position=c(0.2,0.8),
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black",fill="white", linewidth=1),
    axis.text.x=element_text(size = 35, vjust=0.8),axis.text.y=element_text(size = 35),
    axis.line = element_line(colour = "black"))
#ggsave('fig4C.svg', width=8.7, height=8.9, path = savepath)

##################
##Supplementary###
##Fig. S13 & S14##
##################
dt_clean_supp<-dt %>% group_by(Condition, Name, Occupation, Gender) %>% slice_sample(n=20)
dt_clean_supp<-subset(dt_clean_supp, Total.Experience>0)

#Fig. S13A
ggplot(dt_clean_supp, aes(x = age, y = age_since_grad)) + theme_bw() +
  geom_point(color = "grey50", stroke=2, alpha = 0.1, size=15) + 
  xlab("Resume Age") + ylab("Years since Graduation") + theme(
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
              linewidth = 1, color="red", data = dt_clean)

#Fig. S13B
ggplot(dt_clean_supp, aes(x = age, y = Total.Experience)) + theme_bw() +
  geom_point(color = "grey50", stroke=1, alpha = 0.1, size=15) + 
  xlab("Resume Age") + ylab("Applicant's Total Years of Experience") + theme(
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
              linewidth = 1, color="red", data = dt_clean)

##########
#FIG. S14#
##########
scores_SI_dt<-read.csv(paste(datapath, "resume_scores_temp_SI.csv", sep=""))
scores_SI_dt<-subset(scores_SI_dt, score <= 100)
scores_SI_dt$config<-paste(scores_SI_dt$prompt, scores_SI_dt$temp, sep="_")
scores_SI_dt<-subset(scores_SI_dt, Total.Experience>0) #robust to including

cor.test(scores_SI_dt$age, scores_SI_dt$score)
cor.test(scores_SI_dt$Total.Experience, scores_SI_dt$score)
cor.test(scores_SI_dt$age_since_grad, scores_SI_dt$score)

#############
mod<-lm(score ~ Gender + age + Total.Experience + Name  + Occupation + temp + prompt, data = scores_SI_dt)
summary(mod)

mod_int<-lm(score ~ Gender * age + Total.Experience + Name  + Occupation + temp + prompt, data = scores_SI_dt)
summary(mod_int)

scores_SI_dt_treat<-subset(scores_SI_dt, Condition=="Treatment")

mod_int_treatment<-lm(score ~ Gender * age + Total.Experience + Name  + Occupation + temp + prompt, data = scores_SI_dt_treat)
summary(mod_int_treatment)

scores_SI_dt_treat_corr_agg<-scores_SI_dt_treat %>% group_by(temp, prompt) %>% 
  dplyr::summarise(
    age_score_r = as.numeric(cor.test(age, score)$estimate),
    age_score_p = as.numeric(cor.test(age, score)$p.value), 
    
    exp_score_r = as.numeric(cor.test(Total.Experience, score)$estimate),
    exp_score_p = as.numeric(cor.test(Total.Experience, score)$p.value), 
    
    grad_score_r = as.numeric(cor.test(age_since_grad, score)$estimate),
    grad_score_p = as.numeric(cor.test(age_since_grad, score)$p.value) 
  )

scores_SI_dt_treat_corr_agg_long <- gather(scores_SI_dt_treat_corr_agg, condition, measurement, age_score_r:grad_score_p, factor_key=TRUE)

scores_SI_dt_treat_corr_agg_long<-scores_SI_dt_treat_corr_agg_long %>% dplyr::mutate(measure = grepl("_r", condition))

scores_SI_dt_treat_corr_agg_long$condition<-as.character(scores_SI_dt_treat_corr_agg_long$condition)

scores_SI_dt_treat_corr_agg_long$correlation<-sapply(1:nrow(scores_SI_dt_treat_corr_agg_long), 
                                                     function(x) 
                                                       paste(strsplit(scores_SI_dt_treat_corr_agg_long[x,]$condition, "_")[[1]][1],
                                                             strsplit(scores_SI_dt_treat_corr_agg_long[x,]$condition, "_")[[1]][2], sep="_"))

scores_SI_dt_treat_corr_agg_long$measure<-as.factor(scores_SI_dt_treat_corr_agg_long$measure)
levels(scores_SI_dt_treat_corr_agg_long$measure)<-c("pval", "corr")
scores_SI_dt_treat_corr_agg_long$condition<-paste(scores_SI_dt_treat_corr_agg_long$prompt, scores_SI_dt_treat_corr_agg_long$temp, sep="_")


scores_SI_dt_treat %>% group_by(temp) %>% dplyr::summarise(numresumes = length(unique(Resume)))

###########
#FIG. S14A#
###########
SI_model_temp_corr_plot<-subset(scores_SI_dt_treat_corr_agg_long, grepl("basic", condition))
SI_model_temp_corr_plot$condition<-as.factor(SI_model_temp_corr_plot$condition)
levels(SI_model_temp_corr_plot$condition)<-c("0.7", "1.7", "0.3")
SI_model_temp_corr_plot$condition<-factor(SI_model_temp_corr_plot$condition, levels=c("0.3", "0.7", "1.7"))
SI_model_temp_corr_plot$correlation<-as.factor(SI_model_temp_corr_plot$correlation)
levels(SI_model_temp_corr_plot$correlation)<-c("Age", "Experience", "Graduation")

ggplot(subset(SI_model_temp_corr_plot, measure == "corr"), aes(x = measurement, y = condition, shape = correlation)) + theme_bw() +
  geom_point(size=15) + xlab("Pearson Correlation with Resume Score") + ylab("Temperature") + 
  theme(
    axis.title.x=element_text(size=35),
    axis.title.y=element_text(size=35),
    plot.title=element_text(size=35, hjust=0.5), 
    legend.text=element_text(size=35),
    legend.title=element_blank(),
    legend.position="top",
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black",fill="white", linewidth=1.4),
    axis.text.x=element_text(size = 30, vjust=0.8),axis.text.y=element_text(size = 30),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  geom_vline(xintercept = 0, linetype="dotted", linewidth=2)

###########
#FIG. S14B#
###########
mod_basic3<-lm(score ~ Gender * age + Total.Experience + Name  + Occupation, data = subset(scores_SI_dt_treat, config == "basic_3"))
mod_basic3_sum<-summary(mod_basic3)
mod_basic3_df<-as.data.frame(mod_basic3_sum$coefficients)
mod_basic3_df$variable<-row.names(mod_basic3_df)
mod_basic3_df$config<-"basic_3"

mod_basic10<-lm(score ~ Gender * age + Total.Experience + Name  + Occupation, data = subset(scores_SI_dt_treat, config == "basic_10"))
mod_basic10_sum<-summary(mod_basic10)
mod_basic10_df<-as.data.frame(mod_basic10_sum$coefficients)
mod_basic10_df$variable<-row.names(mod_basic10_df)
mod_basic10_df$config<-"basic_10"

mod_basic17<-lm(score ~ Gender * age + Total.Experience + Name  + Occupation, data = subset(scores_SI_dt_treat, config == "basic_17"))
mod_basic17_sum<-summary(mod_basic17)
mod_basic17_df<-as.data.frame(mod_basic17_sum$coefficients)
mod_basic17_df$variable<-row.names(mod_basic17_df)
mod_basic17_df$config<-"basic_17"

mod_long3<-lm(score ~ Gender * age + Total.Experience + Name  + Occupation, data = subset(scores_SI_dt_treat, config == "long_3"))
mod_long3_sum<-summary(mod_long3)
mod_long3_df<-as.data.frame(mod_long3_sum$coefficients)
mod_long3_df$variable<-row.names(mod_long3_df)
mod_long3_df$config<-"long_3"

mod_long10<-lm(score ~ Gender * age + Total.Experience + Name  + Occupation, data = subset(scores_SI_dt_treat, config == "long_10"))
mod_long10_sum<-summary(mod_long10)
mod_long10_df<-as.data.frame(mod_long10_sum$coefficients)
mod_long10_df$variable<-row.names(mod_long10_df)
mod_long10_df$config<-"long_10"

mod_long17<-lm(score ~ Gender * age + Total.Experience + Name  + Occupation, data = subset(scores_SI_dt_treat, config == "long_17"))
mod_long17_sum<-summary(mod_long17)
mod_long17_df<-as.data.frame(mod_long17_sum$coefficients)
mod_long17_df$variable<-row.names(mod_long17_df)
mod_long17_df$config<-"long_17"

plot_ints<-rbind(
  subset(mod_basic3_df, variable == "GenderMale:age"), 
  subset(mod_basic10_df, variable == "GenderMale:age"), 
  subset(mod_basic17_df, variable == "GenderMale:age"),
  subset(mod_long3_df, variable == "GenderMale:age"), 
  subset(mod_long10_df, variable == "GenderMale:age"), 
  subset(mod_long17_df, variable == "GenderMale:age")
)

plot_ints$xmin<-plot_ints$Estimate - plot_ints$`Std. Error`
plot_ints$xmax<-plot_ints$Estimate + plot_ints$`Std. Error`

Figs14_panelB_plot<-subset(plot_ints, grepl("basic", config))
Figs14_panelB_plot$config<-as.character(Figs14_panelB_plot$config)
Figs14_panelB_plot$config<-as.factor(Figs14_panelB_plot$config)
levels(Figs14_panelB_plot$config)<-c("0.7", "1.7", "0.3")
Figs14_panelB_plot$config<-factor(Figs14_panelB_plot$config, levels=c("0.3", "0.7", "1.7"))

ggplot(Figs14_panelB_plot, aes(x = Estimate, y = config, xmin=xmin, xmax = xmax)) + theme_bw() +
  geom_point(size=10) + geom_errorbar(width=0.2, linewidth=2) + 
  ggtitle("Gender[Male] x Age") + 
  xlab("Partial Correlation with Resume Score") + ylab("Temperature") + theme(
    axis.title.x=element_text(size=35),
    axis.title.y=element_text(size=35),
    plot.title=element_text(size=35, hjust=0.5), 
    legend.text=element_text(size=35),
    legend.title=element_blank(),
    legend.position="top",
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black",fill="white", linewidth=1.4),
    axis.text.x=element_text(size = 30, vjust=0.8),axis.text.y=element_text(size = 30),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  geom_vline(xintercept = 0, linetype="dotted", linewidth=2) + 
  coord_cartesian(xlim=c(0, 0.05))

###################
#Merge with Census#
###################
census_map<-read.csv(paste(datapath, "census_map.csv", sep=""))
dt_clean_agg<-dt_clean %>% group_by(Condition, Occupation) %>% dplyr::summarise(age = mean(age, na.rm=T))
dt_clean_agg$category<-gsub(" ", "",dt_clean_agg$Occupation, fixed = TRUE)

dt_clean_agg_m<-merge(dt_clean_agg, census_map, by=c("category"))
dt_clean_agg_m_long <- gather(dt_clean_agg_m, census_year, census_age, Age_2020:Age_2023, factor_key=TRUE)
cor.test(dt_clean_agg_m_long$age, dt_clean_agg_m_long$census_age)

alpha <- 0.05                      # Significance level for 95% CI
z_value <- qnorm(1 - alpha / 2) 

tableS16<-lm(age ~ census_age + census_year + condition, data = dt_clean_agg_m_long)
summary(tableS16)

tableS16_cluster_se <- vcovCL(tableS16, cluster = ~Occupation)
tableS16_clustered_mod <- coeftest(tableS16, vcov = tableS16_cluster_se)
print(tableS16_clustered_mod)
tableS16_clustered_coef_values <- tableS16_clustered_mod[, 1]           # Coefficients
tableS16_clustered_se_values <- tableS16_clustered_mod[, 2]             # Standard errors

# Confidence intervals
tableS16_clustered_lower_bound <- tableS16_clustered_coef_values - z_value * tableS16_clustered_se_values
tableS16_clustered_upper_bound <- tableS16_clustered_coef_values + z_value * tableS16_clustered_se_values
tableS16_clustered_est_w_CI <- data.frame(
  Estimate = tableS16_clustered_coef_values,
  `Lower Bound` = tableS16_clustered_lower_bound,
  `Upper Bound` = tableS16_clustered_upper_bound
)

tableS16_tbl <- tbl_regression(tableS16, pvalue_fun = ~format(.x, scientific = TRUE)) %>% 
  modify_header(label = "**Variables**") %>% 
  add_glance_table(
    include = c(statistic, r.squared, adj.r.squared, nobs, df, AIC, df.residual, sigma) #everything()
  ) 

as_flex_table(tableS16_tbl) %>% flextable::save_as_docx(path = paste(savepath, "table_S16.docx", sep=""))





