rm(list=ls());gc()
library(dplyr); library(ggplot2); library(tidyverse);
library(lubridate); library(sjPlot); library(sjmisc)
library(lmtest); library(sandwich); library(gtsummary)
library(flextable)
theme_set(theme_sjplot())

########################
#Extended Data Figure 1#
########################
medage_by_perc_women<-read.csv("G:/My Drive/Research/Labs/COMPSYN/gendered ageism/data/nature_submit/full_perc_women_age.csv")
colnames(medage_by_perc_women)<-c("Occupation", "Total", "Women", "Age", "Census.Year")
medage_by_perc_women$Women<-as.numeric(medage_by_perc_women$Women)
medage_by_perc_women$Age<-as.numeric(medage_by_perc_women$Age)
medage_by_perc_women$Census.Year<-as.character(medage_by_perc_women$Census.Year)

tableS1<-lm(Age ~ Women + Census.Year, data = medage_by_perc_women)
summary(tableS1)

tableS1_clust_se <- vcovCL(tableS1, cluster = ~ Occupation)
coeftest(tableS1, vcov = tableS1_clust_se)
confint(coeftest(tableS1, vcov = tableS1_clust_se))

ggplot(subset(medage_by_perc_women, Census.Year == "2012"),  
       aes(x = Women, y = Age)) + theme_bw() +
  geom_point(size =3) + 
  geom_smooth(method='lm', formula= y~x) + 
  ggtitle("2012") + 
  xlab("% Women in Occupation") + ylab("Median Age") + theme(
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
  coord_cartesian(ylim=c(20,60))

ggplot(subset(medage_by_perc_women, Census.Year == "2023"),  
       aes(x = Women, y = Age)) + theme_bw() +
  geom_point(size =3) + 
  geom_smooth(method='lm', formula= y~x) + 
  ggtitle("2023") + 
  xlab("% Women in Occupation") + ylab("Median Age") + theme(
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
  coord_cartesian(ylim=c(20,60))

############################
#Extended Data Figure 2 & 3#
############################
industrylevel<-read.csv("G:/My Drive/Research/Labs/COMPSYN/gendered ageism/data/nature_submit/industry_match.csv")

industrylevel_agg<-industrylevel %>% group_by(Industry, Data.Source, Img.Gender.Mode, sex, year) %>% 
  dplyr::summarise(Img.Age.Avg = mean(Img.Age.Avg), age = mean(age), age_norm = mean(age_norm))

industrylevel_agg_long<-
  rbind(
    data.frame(source = "Google", year = 2023, gender = industrylevel_agg$sex, 
               age = industrylevel_agg$Img.Age.Avg, 
               Industry = industrylevel_agg$Industry),
    data.frame(source = "Census", year = industrylevel_agg$year, 
               gender = industrylevel_agg$sex, 
               age = industrylevel_agg$age_norm,
               Industry = industrylevel_agg$Industry)
  )

industrylevel_agg_long$source_year<-paste(industrylevel_agg_long$source, industrylevel_agg_long$year, sep="_")

#Extended Data Figure 2
ggplot(industrylevel_agg_long, 
       aes(x = Industry, y = age, color = gender, shape=source_year, group=source_year)) + theme_bw() +
  geom_point(size=10, position = position_dodge(0.8), stroke = 3) + 
  scale_color_manual(values=c("gold", "dodgerblue")) + 
  scale_shape_manual(values=0:12) + 
  xlab("Industry") + ylab("Age (Binned)") + theme(
    axis.title.x=element_text(size=30),
    axis.title.y=element_text(size=30),
    plot.title=element_text(size=30, hjust=0.5),
    legend.text=element_text(size=20),
    legend.title=element_blank(),
    legend.background = element_blank(),
    legend.position = "top",
    legend.box.background = element_rect(colour = "black",fill="white", linewidth=1.4),
    axis.text.x=element_text(size = 26, vjust=0.8),
    axis.text.y=element_text(size = 30),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    panel.background = element_blank(), axis.line = element_line(colour = "black")) 

industrylevel_agg_long_delta<-industrylevel_agg_long %>% group_by(source, year, Industry, source_year) %>% 
  reframe(age_gap = abs(age[gender=="Male"] - age[gender=="Female"]), 
          Male_older = age[gender=="Male"] > age[gender=="Female"], 
          Male_age = age[gender=="Male"], 
          Female_age = age[gender=="Female"],
          midpoint = mean(c(Male_age, Female_age)))

industrylevel_agg_long_delta<-unique(industrylevel_agg_long_delta)
industrylevel_agg_long_delta<-industrylevel_agg_long_delta %>% arrange(Industry, -age_gap)
industrylevel_agg_long_delta$Male_age_cent<-industrylevel_agg_long_delta$Male_age - industrylevel_agg_long_delta$midpoint
industrylevel_agg_long_delta$Female_age_cent<-industrylevel_agg_long_delta$Female_age - industrylevel_agg_long_delta$midpoint

industrylevel_agg_long_delta_plot <- 
  gather(industrylevel_agg_long_delta, gender_measure, plot_point, Male_age_cent:Female_age_cent, factor_key=TRUE)

industrylevel_agg_long_delta_plot$gender_measure<-as.factor(industrylevel_agg_long_delta_plot$gender_measure)
levels(industrylevel_agg_long_delta_plot$gender_measure)<-c("Male", "Female")

###Sales
sales_plot<-subset(industrylevel_agg_long_delta_plot, Industry == "Sales")

ggplot(sales_plot, aes(x = plot_point, y = reorder(source_year, age_gap), color = gender_measure, linetype=Male_older)) + 
  ggtitle("Sales") + theme_bw() +
  scale_color_manual(values=c("dodgerblue", "gold")) + 
  geom_point(size=10, stroke = 3) + 
  geom_line(linewidth=2, color="black") +
  scale_linetype_manual(values = c("FALSE" = "dotted", "TRUE" = "solid")) +
  xlab("Age Gap") + ylab("Source") + theme(
    axis.title.x=element_text(size=30),
    axis.title.y=element_blank(),
    plot.title=element_text(size=30, hjust=0.5),
    legend.text=element_text(size=30),
    legend.title=element_blank(),
    legend.background = element_blank(),
    legend.position = "none",
    legend.box.background = element_rect(colour = "black",fill="white", linewidth=1.4),
    axis.text.x=element_text(size = 30, vjust=0.8),
    axis.text.y=element_text(size = 30),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    panel.background = element_blank(), axis.line = element_line(colour = "black"),
    aspect.ratio = 1) + 
  geom_vline(xintercept = 0) + 
  coord_cartesian(xlim=c(-0.25,0.25))

###Management
mgmt_plot<-subset(industrylevel_agg_long_delta_plot, Industry == "Management")

ggplot(mgmt_plot, aes(x = plot_point, y = reorder(source_year, age_gap), color = gender_measure, linetype=Male_older)) + 
  ggtitle("Management") + theme_bw() +
  scale_color_manual(values=c("dodgerblue", "gold")) + 
  geom_point(size=10, stroke = 3) + 
  geom_line(linewidth=2, color="black") +
  scale_linetype_manual(values = c("FALSE" = "dotted", "TRUE" = "solid")) +
  xlab("Age Gap") + ylab("Source") + theme(
    axis.title.x=element_text(size=30),
    axis.title.y=element_blank(),
    plot.title=element_text(size=30, hjust=0.5),
    legend.text=element_text(size=30),
    legend.title=element_blank(),
    legend.background = element_blank(),
    legend.position = "none",
    legend.box.background = element_rect(colour = "black",fill="white", linewidth=1.4),
    axis.text.x=element_text(size = 30, vjust=0.8),
    axis.text.y=element_text(size = 30),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    panel.background = element_blank(), axis.line = element_line(colour = "black"),
    aspect.ratio = 1) + 
  geom_vline(xintercept = 0) + 
  coord_cartesian(xlim=c(-0.25,0.25))


###Service
service_plot<-subset(industrylevel_agg_long_delta_plot, Industry == "Service")

ggplot(service_plot, aes(x = plot_point, y = reorder(source_year, age_gap), color = gender_measure, linetype=Male_older)) + 
  ggtitle("Service") + theme_bw() +
  scale_color_manual(values=c("dodgerblue", "gold")) + 
  geom_point(size=10, stroke = 3) + 
  geom_line(linewidth=2, color="black") +
  scale_linetype_manual(values = c("FALSE" = "dotted", "TRUE" = "solid")) +
  xlab("Age Gap") + ylab("Source") + theme(
    axis.title.x=element_text(size=30),
    axis.title.y=element_blank(),
    plot.title=element_text(size=30, hjust=0.5),
    legend.text=element_text(size=30),
    legend.title=element_blank(),
    legend.background = element_blank(),
    legend.position = "none",
    legend.box.background = element_rect(colour = "black",fill="white", linewidth=1.4),
    axis.text.x=element_text(size = 30, vjust=0.8),
    axis.text.y=element_text(size = 30),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    panel.background = element_blank(), axis.line = element_line(colour = "black"), 
    aspect.ratio = 1) + 
  geom_vline(xintercept = 0) + 
  coord_cartesian(xlim=c(-0.25,0.25))

####Production
production_plot<-subset(industrylevel_agg_long_delta_plot, Industry == "Production")

ggplot(production_plot, aes(x = plot_point, y = reorder(source_year, age_gap), color = gender_measure, linetype=Male_older)) + 
  ggtitle("Production") + theme_bw() +
  scale_color_manual(values=c("dodgerblue", "gold")) + 
  geom_point(size=10, stroke = 3) + 
  geom_line(linewidth=2, color="black") +
  scale_linetype_manual(values = c("FALSE" = "dotted", "TRUE" = "solid")) +
  xlab("Age Gap") + ylab("Source") + theme(
    axis.title.x=element_text(size=30),
    axis.title.y=element_blank(),
    plot.title=element_text(size=30, hjust=0.5),
    legend.text=element_text(size=30),
    legend.title=element_blank(),
    legend.background = element_blank(),
    legend.position = "none",
    legend.box.background = element_rect(colour = "black",fill="white", linewidth=1.4),
    axis.text.x=element_text(size = 30, vjust=0.8),
    axis.text.y=element_text(size = 30),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    panel.background = element_blank(), axis.line = element_line(colour = "black"),
    aspect.ratio = 1) + 
  geom_vline(xintercept = 0) + 
  coord_cartesian(xlim=c(-0.25,0.25))

####Production
Resources_plot<-subset(industrylevel_agg_long_delta_plot, Industry == "Resources")

ggplot(Resources_plot, aes(x = plot_point, y = reorder(source_year, age_gap), color = gender_measure, linetype=Male_older)) + 
  ggtitle("Resources") + theme_bw() +
  scale_color_manual(values=c("dodgerblue", "gold")) + 
  geom_point(size=10, stroke = 3) + 
  geom_line(linewidth=2, color="black") +
  scale_linetype_manual(values = c("FALSE" = "dotted", "TRUE" = "solid")) +
  xlab("Age Gap") + ylab("Source") + theme(
    axis.title.x=element_text(size=30),
    axis.title.y=element_blank(),
    plot.title=element_text(size=30, hjust=0.5),
    legend.text=element_text(size=30),
    legend.title=element_blank(),
    legend.background = element_blank(),
    legend.position = "none",
    legend.box.background = element_rect(colour = "black",fill="white", linewidth=1.4),
    axis.text.x=element_text(size = 30, vjust=0.8),
    axis.text.y=element_text(size = 30),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    panel.background = element_blank(), axis.line = element_line(colour = "black"), 
    aspect.ratio = 1) + 
  geom_vline(xintercept = 0) + 
  coord_cartesian(xlim=c(-0.25,0.25))

#########
#Fig. S1#
#########
census_whole_pop<-read.csv("G:/My Drive/Research/Labs/COMPSYN/gendered ageism/data/nature_submit/gender_age_whole_pop.csv")

census_whole_pop_median<-subset(census_whole_pop, age == "Median age")
census_whole_pop_median<-census_whole_pop_median %>% dplyr::select(-male_percent, -female_percent)
census_whole_pop_median_long<-rbind(
  data.frame(year = census_whole_pop_median$year, age = census_whole_pop_median$male_number, gender="Male"), 
  data.frame(year = census_whole_pop_median$year, age = census_whole_pop_median$female_number, gender="Female")
)

ggplot(census_whole_pop_median_long, aes(x = as.factor(year), y = age, color=gender, group=gender)) + theme_bw() +
  geom_point(size = 10) + geom_line(linewidth=1) + 
  scale_color_manual(values = c("goldenrod", "dodgerblue")) + 
  xlab("Census Year") + ylab("Median Age") + theme(
    axis.title.x=element_text(size=35),
    axis.title.y=element_text(size=35),
    plot.title=element_text(size=35, hjust=0.5),
    legend.text=element_text(size=35),
    legend.title=element_blank(),
    legend.position="top",
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black",fill="white", linewidth=1.4),
    axis.text.x=element_text(size = 30, vjust=0.8, angle=25),axis.text.y=element_text(size = 30),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    panel.background = element_blank(), axis.line = element_line(colour = "black"))

####################
#Fig. S2 & Table S2#
####################
dt_gender_workforce<-read.csv("G:/My Drive/Research/Labs/COMPSYN/gendered ageism/data/nature_submit/dt_gender_workforce.csv")
dt_gender_workforce$age_num<-as.factor(dt_gender_workforce$age)
levels(dt_gender_workforce$age_num)<-1:length(levels(dt_gender_workforce$age_num))
dt_gender_workforce$age_num<-as.numeric(as.character(dt_gender_workforce$age_num))

dt_gender_workforce_agg<-dt_gender_workforce %>% group_by(gender, year) %>% 
  dplyr::summarise(avg_age = weighted.mean(age_num, percent, na.rm=T))

dt_gender_workforce_agg_corr<-dt_gender_workforce %>% group_by(year) %>% 
  dplyr::summarise(
    diff = t.test(percent[gender=="male"], percent[gender=="female"], paired=T)$estimate,
    pval = t.test(percent[gender=="male"], percent[gender=="female"], paired=T)$p.value, 
    cilow = t.test(percent[gender=="male"], percent[gender=="female"], paired=T)$conf.int[1], 
    cihi = t.test(percent[gender=="male"], percent[gender=="female"], paired=T)$conf.int[2]
  )

dt_gender_workforce$age_dist<-dt_gender_workforce$number * dt_gender_workforce$age_num

dt_gender_workforce_agg_raw<-dt_gender_workforce %>% group_by(year) %>% 
  dplyr::reframe(
    diff = t.test(age_dist[gender=="male"], age_dist[gender=="female"])$estimate,
    pval = t.test(age_dist[gender=="male"], age_dist[gender=="female"])$p.value, 
    cilow = t.test(age_dist[gender=="male"], age_dist[gender=="female"])$conf.int[1], 
    cihi = t.test(age_dist[gender=="male"], age_dist[gender=="female"])$conf.int[2]
  )

dt_gender_workforce_agg_raw<-dt_gender_workforce_agg_raw %>% group_by(year) %>% 
  dplyr::reframe(diff = mean(diff), pval=mean(pval), cilow=mean(cilow), cihi=mean(cihi))

dt_gender_workforce$age_num_cat<-as.factor(dt_gender_workforce$age_num)
dt_gender_workforce$year<-as.factor(dt_gender_workforce$year)
dt_gender_workforce$gender<-as.factor(dt_gender_workforce$gender)
levels(dt_gender_workforce$gender)<-c("Female", "Male")

tableS2<-lm(percent ~ gender * age + year, data = dt_gender_workforce)
summary(tableS2)

tableS2_tbl <- tbl_regression(tableS2, pvalue_fun = ~format(.x, scientific = TRUE)) %>% 
  modify_header(label = "**Variables**") %>% 
  add_glance_table(
    include = c(statistic, r.squared, adj.r.squared, nobs, df, AIC, df.residual, sigma) #everything()
  )

plot_model(tableS2, type = "pred", terms = c("age", "gender"), 
           show.p = TRUE, wrap.title = 100, wrap.labels = 100, line.size = 1, 
           grid.breaks = FALSE, value.size=4, show.data = TRUE, 
           dot.size=6, ci.style = "whisker", dot.alpha = 0.2) + theme_bw() + 
  xlab("Age Group") + 
  ylab("Employed") + 
  theme(
    axis.title.x=element_text(size=30),
    axis.title.y=element_text(size=30),
    plot.title=element_blank(), 
    legend.text=element_text(size=30),
    legend.title=element_blank(),
    legend.position="top",
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black",fill="white", linewidth=1.4),
    axis.text.x=element_text(size = 25, vjust=0.8),axis.text.y=element_text(size = 30),
    axis.line = element_line(colour = "black")) + 
  geom_line(linewidth=1) + 
  scale_color_manual(values=c("dodgerblue", "goldenrod")) + 
  coord_cartesian(ylim=c(0, 0.125)) + 
  scale_y_continuous(labels = scales::percent)
