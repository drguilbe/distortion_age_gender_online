rm(list=ls());gc()
library(dplyr)
library(ggplot2)
library(tidyverse)

#Functions
min_max_norm<-function(x){(x - min(x,na.rm=T))/(max(x,na.rm=T) - min(x,na.rm=T))}

windows()
savepath=""

#########
#FIG S10#
#########
dim_mod<-read.csv("G:/My Drive/Research/Labs/COMPSYN/gendered ageism/data/Nature_Submit/GPT2-large-dimensions.csv")
dim_mod_simp<-dim_mod %>% dplyr::select(-Social.Category, -Source, -gender.main, -gender.ext, -gender.red, -age.main, -age.ext, -age.red)
dim_corr_matrix <- dim_mod_simp %>% cor(use = "complete.obs") 
long_dim_corr <- melt(dim_corr_matrix)
mean(long_dim_corr$value)

long_dim_corr$Var1<-as.factor(long_dim_corr$Var1)
levels(long_dim_corr$Var1)<-c("Gender.Main", "Gender.Ext", "Gender.Red", "Age.Main", "Age.Ext", "Age.Red")
long_dim_corr$Var2<-as.factor(long_dim_corr$Var2)
levels(long_dim_corr$Var2)<-c("Gender.Main", "Gender.Ext", "Gender.Red", "Age.Main", "Age.Ext", "Age.Red")

ggplot(long_dim_corr, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +  theme_bw() + # Add white lines to distinguish the tiles
  geom_text(aes(label = round(value, 2)), size = 10) +  # Add correlation numbers to each cell
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name="Correlation") +
  labs(x = "Model", y = "Model", title = "Pairwise Model Correlation Heatmap") + 
  theme(axis.text.x = element_text(size = 19), 
        axis.text.y = element_text(size=19),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        legend.text=element_text(size=20), 
        legend.title=element_text(size=20), 
        plot.title = element_blank()) 

#ggsave("dimension_robustness.png", width=13, height=13, path = savepath)

#########
#FIG S11#
#########
dt_all<-read.csv("G:/My Drive/Research/Labs/COMPSYN/gendered ageism/data/Nature_Submit/all_lang_model_dim_data.csv")

savepath<-""

dt_agg<-dt_all %>% group_by(model, Age.Bin) %>% 
  dplyr::summarise(gender=mean(Gender, na.rm=T), 
                   age=mean(Age, na.rm=T))

dt_agg<-dt_agg %>% group_by(model) %>% dplyr::mutate(gender_norm=min_max_norm(gender))

#pdf(file = paste0(savepath, 'all_LLMs_age_gender_association.pdf'), width = 18, height = 18, useDingbats = FALSE)
ggplot(dt_agg, aes(x=Age.Bin, y=gender_norm, color=model, fill=model))+
  geom_point(size=12, alpha=0.4) + 
  geom_smooth(linewidth=3, se=F, method = 'lm') +
  xlab("Age Association\n(Young to Old | Binned)") + ylab("Male Bias") + theme_bw() + 
  theme(axis.text.x = element_text(size=50), axis.text.y = element_text(size=50),
        axis.title.x = element_text(size=50),axis.title.y = element_text(size=50),
        plot.title = element_blank(), 
        legend.position = "top", 
        legend.text=element_text(size=30),
        legend.title=element_blank(),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  scale_x_continuous(breaks=c(0,5,10,15,20), limits=c(0,20))
#dev.off()

cor.test(word2vec_2013_s$Age, word2vec_2013_s$Gender); nrow(word2vec_2013_s)
cor.test(retrained_s$Age, retrained_s$Gender); nrow(retrained_s)
cor.test(twitter_200D_s$Age, twitter_200D_s$Gender); nrow(twitter_200D_s)
cor.test(wiki_300D_s$Age, wiki_300D_s$Gender); nrow(wiki_300D_s)
cor.test(fasttext_s$Age, fasttext_s$Gender); nrow(fasttext_s)
cor.test(bert_s$Age, bert_s$Gender); nrow(bert_s)
cor.test(roberta_s$Age, roberta_s$Gender); nrow(roberta_s)
cor.test(gpt2_s$Age, gpt2_s$Gender); nrow(gpt2_s)
cor.test(gpt4_s$Age, gpt4_s$Gender); nrow(gpt4_s)

cor.test(subset(dt_agg, model=="word2vec.2013")$Age.Bin, subset(dt_agg, model=="word2vec.2013")$gender_norm)
cor.test(subset(dt_agg, model=="word2vec.2023")$Age.Bin, subset(dt_agg, model=="word2vec.2023")$gender_norm)
cor.test(subset(dt_agg, model=="Twitter.Glove")$Age.Bin, subset(dt_agg, model=="Twitter.Glove")$gender_norm)
cor.test(subset(dt_agg, model=="Wiki.Glove")$Age.Bin, subset(dt_agg, model=="Wiki.Glove")$gender_norm)
cor.test(subset(dt_agg, model=="Wiki.Fasttext")$Age.Bin, subset(dt_agg, model=="Wiki.Fasttext")$gender_norm)
cor.test(subset(dt_agg, model=="BERT")$Age.Bin, subset(dt_agg, model=="BERT")$gender_norm)
cor.test(subset(dt_agg, model=="Roberta")$Age.Bin, subset(dt_agg, model=="Roberta")$gender_norm)
cor.test(subset(dt_agg, model=="GPT2-Large")$Age.Bin, subset(dt_agg, model=="GPT2-Large")$gender_norm)
cor.test(subset(dt_agg, model=="GPT4")$Age.Bin, subset(dt_agg, model=="GPT4")$gender_norm)

##########
#FIG. S12#
##########
dt_age_final<-read.csv("G:/My Drive/Research/Labs/COMPSYN/gendered ageism/data/Nature_Submit/all_lang_model_dim_data_AGE_SI.csv")
dt_age_final<-dt_age_final %>% group_by(Social.Category) %>% dplyr::mutate(number_models = length(unique(model)))
dt_age_final_match<-subset(dt_age_final, number_models == 9)
dt_age_final_match<-dt_age_final_match %>% arrange(model, Social.Category)

dt_age_final_match_wide<-data.frame(
  Social.Category=subset(dt_age_final_match, model == "BERT")$Social.Category, 
  BERT=subset(dt_age_final_match, model == "BERT")$Age, 
  GPT2Large=subset(dt_age_final_match, model == "GPT2-Large")$Age, 
  GPT4=subset(dt_age_final_match, model == "GPT4")$Age, 
  Roberta=subset(dt_age_final_match, model == "Roberta")$Age, 
  TwitterGlove=subset(dt_age_final_match, model == "Twitter.Glove")$Age,
  WikiFasttext=subset(dt_age_final_match, model == "Wiki.Fasttext")$Age, 
  WikiGlove=subset(dt_age_final_match, model == "Wiki.Glove")$Age, 
  word2vec2013=subset(dt_age_final_match, model == "word2vec.2013")$Age, 
  word2vec2023=subset(dt_age_final_match, model == "word2vec.2023")$Age
)

dt_age_final_match_corr_matrix <- dt_age_final_match_wide %>%
  select(-Social.Category) %>%  # Exclude the non-numeric column
  cor(use = "complete.obs") 

long_age_corr <- melt(dt_age_final_match_corr_matrix)

ggplot(long_age_corr, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +  # Add white lines to distinguish the tiles
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name="Correlation") +
  geom_text(aes(label = round(value, 2)), size = 10) +  # Add correlation numbers to each cell
  theme_bw() +  # Minimal theme
  theme(axis.text.x = element_text(angle = 25, hjust = 1, size=20), 
        axis.text.y = element_text(angle = 0, hjust = 1, size=20), 
        plot.title = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        legend.text = element_text(size=20),
        legend.title = element_text(size=20)) +  # Rotate x axis labels for better visibility
  labs(x = "Model", y = "Model", title = "Pairwise Model Correlation Heatmap")

#ggsave("Age_dimension_correlation.png", width=12, height=12, path = savepath)

#Tables#
gpt2_dt<-read.csv("G:/My Drive/Research/Labs/COMPSYN/gendered ageism/data/Nature_Submit/GPT2-large-dimensions.csv")
Guilbeault2024_data_raw<-read.csv("G:/My Drive/Research/Labs/COMPSYN/gendered ageism/data/gendered_ageism_img/nature_data_obsv.csv")
Guilbeault2024_data_text_dt<-unique(Guilbeault2024_data_raw %>% dplyr::select(Social.Category, Gendered.Category, Polysemy, Word.Frequency.Scaled))
colnames(gpt2_dt)[1]<-"Social.Category"
gpt2_dt_full<-merge(gpt2_dt, Guilbeault2024_data_text_dt, by=c("Social.Category"))

table_S13<-lm(age.main ~ gender.main + Gendered.Category + Polysemy + Word.Frequency.Scaled, data = gpt2_dt_full)
summary(table_S13)

table_S13_tbl <- tbl_regression(table_S13, pvalue_fun = ~format(.x, scientific = TRUE)) %>% 
  modify_header(label = "**Variables**") %>% 
  add_glance_table(
    include = c(statistic, r.squared, adj.r.squared, nobs, df, AIC, df.residual, sigma) #everything()
  ) 

as_flex_table(table_S13_tbl) %>% flextable::save_as_docx(path = paste(savepath, "table_S13.docx", sep=""))

###################
#merge with census# 
###################
gpt2_dt<-read.csv("G:/My Drive/Research/Labs/COMPSYN/gendered ageism/data/Nature_Submit/GPT2-large-dt.csv")
colnames(gpt2_dt)[1]<-"Social.Category"
min_gender<-min(gpt2_dt$Gender.Score * -1)
max_gender<-max(gpt2_dt$Gender.Score * -1)
gpt2_dt$gender_norm<-sapply(1:nrow(gpt2_dt), function(x) min_max_norm(gpt2_dt[x,]$Gender.Score * -1, min_gender, max_gender))
min_age<-min(gpt2_dt$Age.Score * -1)
max_age<-max(gpt2_dt$Age.Score * -1)
gpt2_dt$age_norm<-sapply(1:nrow(gpt2_dt), function(x) min_max_norm(gpt2_dt[x,]$Age.Score * -1, min_age, max_age))
census_map_f<-read.csv("G:/My Drive/Research/Labs/COMPSYN/gendered ageism/data/Nature_Submit/GPT2-large-census-map.csv") 
gpt2_dt_census<-merge(gpt2_dt, census_map_f, by=c("Social.Category"))

cor.test(gpt2_dt_census$age_norm, gpt2_dt_census$Age_2020)
cor.test(gpt2_dt_census$age_norm, gpt2_dt_census$Age_2021)
cor.test(gpt2_dt_census$age_norm, gpt2_dt_census$Age_2022)
cor.test(gpt2_dt_census$age_norm, gpt2_dt_census$Age_2023)
cor.test(gpt2_dt_census$Age.Score, gpt2_dt_census$Age_2020)
cor.test(gpt2_dt_census$Age.Score, gpt2_dt_census$Age_2021)
cor.test(gpt2_dt_census$Age.Score, gpt2_dt_census$Age_2022)
cor.test(gpt2_dt_census$Age.Score, gpt2_dt_census$Age_2023)
cor.test(gpt2_dt_census$Gender.Score, gpt2_dt_census$Age_2020)
cor.test(gpt2_dt_census$Gender.Score, gpt2_dt_census$Age_2021)
cor.test(gpt2_dt_census$Gender.Score, gpt2_dt_census$Age_2022)
cor.test(gpt2_dt_census$Gender.Score, gpt2_dt_census$Age_2023)

#Census Data Long 
c2020<-gpt2_dt_census %>% select(Social.Category,Age.Score,Gender.Score,Occupation,Age_2020)
colnames(c2020)[5]<-"Census.Age"
c2020$Cyear<-2020
c2021<-gpt2_dt_census %>% select(Social.Category,Age.Score,Gender.Score,Occupation,Age_2021)
colnames(c2021)[5]<-"Census.Age"
c2021$Cyear<-2021
c2022<-gpt2_dt_census %>% select(Social.Category,Age.Score,Gender.Score,Occupation,Age_2022)
colnames(c2022)[5]<-"Census.Age"
c2022$Cyear<-2022
c2023<-gpt2_dt_census %>% select(Social.Category,Age.Score,Gender.Score,Occupation,Age_2023)
colnames(c2023)[5]<-"Census.Age"
c2023$Cyear<-2023

census_GPT2_long<-rbind(c2020, c2021, c2022, c2023)
census_GPT2_long$Cyear<-as.factor(census_GPT2_long$Cyear)

##############
#Model params#
##############
alpha <- 0.05                      # Significance level for 95% CI
z_value <- qnorm(1 - alpha / 2)    # Z-value for 95% CI

####################
##Age Correlations##
####################
cor.test(census_GPT2_long$Age.Score, census_GPT2_long$Census.Age)

table_S14<-lm(Census.Age ~ Age.Score + Cyear, data = census_GPT2_long)
summary(table_S14)
table_S14_cluster_se <- vcovCL(table_S14, cluster = ~Social.Category)
table_S14_clustered_mod <- coeftest(table_S14, vcov = table_S14_cluster_se)
print(table_S14_clustered_mod)
table_S14_clustered_coef_values <- table_S14_clustered_mod[, 1]           # Coefficients
table_S14_clustered_se_values <- table_S14_clustered_mod[, 2]             # Standard errors

# Confidence intervals
table_S14_clustered_lower_bound <- table_S14_clustered_coef_values - z_value * table_S14_clustered_se_values
table_S14_clustered_upper_bound <- table_S14_clustered_coef_values + z_value * table_S14_clustered_se_values
table_S14_clustered_est_w_CI <- data.frame(
  Estimate = table_S14_clustered_coef_values,
  `Lower Bound` = table_S14_clustered_lower_bound,
  `Upper Bound` = table_S14_clustered_upper_bound
)

table_S14_tbl <- tbl_regression(table_S14, pvalue_fun = ~format(.x, scientific = TRUE)) %>% 
  modify_header(label = "**Variables**") %>% 
  add_glance_table(
    include = c(statistic, r.squared, adj.r.squared, nobs, df, AIC, df.residual, sigma) #everything()
  ) 

as_flex_table(table_S14_tbl) %>% flextable::save_as_docx(path = paste(savepath, "table_S14.docx", sep=""))

#######################
##Gender Correlations##
#######################
cor.test(census_GPT2_long$Gender.Score, census_GPT2_long$Census.Age)

table_S15<-lm(Census.Age ~ Gender.Score + Cyear, data = census_GPT2_long)
summary(table_S15)

table_S15_cluster_se <- vcovCL(table_S15, cluster = ~Social.Category)
table_S15_clustered_mod <- coeftest(table_S15, vcov = table_S15_cluster_se)
print(table_S15_clustered_mod)
table_S15_clustered_coef_values <- table_S15_clustered_mod[, 1]           # Coefficients
table_S15_clustered_se_values <- table_S15_clustered_mod[, 2]             # Standard errors

# Confidence intervals
table_S15_clustered_lower_bound <- table_S15_clustered_coef_values - z_value * table_S15_clustered_se_values
table_S15_clustered_upper_bound <- table_S15_clustered_coef_values + z_value * table_S15_clustered_se_values
table_S15_clustered_est_w_CI <- data.frame(
  Estimate = table_S15_clustered_coef_values,
  `Lower Bound` = table_S15_clustered_lower_bound,
  `Upper Bound` = table_S15_clustered_upper_bound
)

table_S15_tbl <- tbl_regression(table_S15, pvalue_fun = ~format(.x, scientific = TRUE)) %>% 
  modify_header(label = "**Variables**") %>% 
  add_glance_table(
    include = c(statistic, r.squared, adj.r.squared, nobs, df, AIC, df.residual, sigma) #everything()
  ) 

as_flex_table(table_S15_tbl) %>% flextable::save_as_docx(path = paste(savepath, "table_S15.docx", sep=""))




