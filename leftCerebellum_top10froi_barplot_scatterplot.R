

# This is the script for plotting barplot for condition (speech and tone) * contrast (structured>random ,and random >structured)
# with no smoothing froi data in cerebellum parcel_2

# rm(list=ls())
library(dplyr);
library(tidyverse);
library(stats);
library(psych);
library(ggpubr);
library(tableone);


## load data
subjectID <- read.csv("/Volumes/lendlab_raid/projects/sllip/make_parcels-tengwen/subjectsID30.csv")
tsl_rand_str <- read.csv("/Volumes/lendlab_raid/projects/sllip/make_parcels-tengwen/data/froi_analysis_results_tsl_rand-str_nosmoothing/julie_stat_learning_contrast_tsl_rand-str_nosmoothing_froi_resp_mag.csv", col.names = 'parcel2_tsl_rand_str')
tsl_str_rand <- read.csv("/Volumes/lendlab_raid/projects/sllip/make_parcels-tengwen/data/froi_analysis_results_tsl_str-rand_nosmoothing/julie_stat_learning_contrast_tsl_str-rand_nosmoothing_froi_resp_mag.csv", col.names = 'parcel2_tsl_str_rand')
ssl_str_rand <- read.csv("/Volumes/lendlab_raid/projects/sllip/make_parcels-tengwen/data/froi_analysis_results_SSL_str-rand_nosmoothing/julie_stat_learning_contrast_SSL_str-rand_nosmoothing_froi_resp_mag.csv", col.names = 'parcel2_ssl_str_rand')
ssl_rand_str <- read.csv("/Volumes/lendlab_raid/projects/sllip/make_parcels-tengwen/data/froi_analysis_results_SSL_rand-str_nosmoothing/julie_stat_learning_contrast_SSL_rand_str_nosmoothing_froi_resp_mag.csv", col.names = 'parcel2_ssl_rand_str')

leftCerebtop10 <- cbind(subjectID, tsl_rand_str, tsl_str_rand, ssl_str_rand, ssl_rand_str)
write.csv(leftCerebtop10, "/Volumes/lendlab_raid/projects/sllip/make_parcels-tengwen/data/leftCerebtop10.csv", row.names = TRUE)

## one-sample t test for both tasks and both rand>str and str>rand
t.test(leftCerebtop10$parcel2_tsl_rand_str,mu = 0,alternative = "two.sided")
t.test(leftCerebtop10$parcel2_tsl_str_rand,mu = 0,alternative = "two.sided")
t.test(leftCerebtop10$parcel2_ssl_str_rand,mu = 0,alternative = "two.sided")
t.test(leftCerebtop10$parcel2_ssl_rand_str,mu = 0,alternative = "two.sided")

##  pivot_longer data
leftCerebtop10_long <- pivot_longer(
  leftCerebtop10,
  cols = c(parcel2_tsl_rand_str, parcel2_tsl_str_rand, parcel2_ssl_str_rand, parcel2_ssl_rand_str),
  names_to = "condition",
  values_to = "activation",
  names_transform = list(condition = as.factor)) %>%
  mutate(subject_ID = as.factor(subject_ID))

# a <- aov(activation ~ condition + Error(subject_ID/condition), data = leftCerebtop10_long)
# summary(a)

mean_sd <- leftCerebtop10_long %>%
  group_by(condition) %>%
  summarize(mean_activation = mean(activation),
            sd = sd(activation),
            se = sd(activation)/sqrt(n()))


## -----------------------------------barplot: separate bars----------------------------

ggplot(mean_sd, aes(x = condition, y = mean_activation)) +
  geom_bar(stat = "identity", position = "dodge",  fill = "lightcoral") +
  geom_errorbar(aes(ymin = mean_activation -se, ymax =mean_activation +se),
                width = 0.1, position = position_dodge(0.9)) +
  theme_bw() 
#   theme_classic()

# -----------------------condition (speech/tone) * contrast (Structured > Random, Random > Structured), grouped barplot------

mean_sd_copy <-mean_sd
mean_sd_copy$contrast <- c("Random > Structured", "Structured > Random","Random > Structured", "Structured > Random")
mean_sd_copy$condition <- c("Speech", "Speech", "Tone", "Tone")
colnames(mean_sd_copy)[2] <- "Mean of Top 10% Activity"
colnames(mean_sd_copy)[1] <- "Condition"
colnames(mean_sd_copy)[5] <- "Contrast"

g = ggplot(mean_sd_copy, aes(x = Condition, y = `Mean of Top 10% Activity`, fill = Contrast)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_sd_copy$`Mean of Top 10% Activity` -se, ymax =mean_sd_copy$`Mean of Top 10% Activity` +se),
                width = 0.2, size = 0.8, position = position_dodge(0.9)) +
  theme_bw() +
  theme(legend.position = c(.8, .92))
  
g + theme(axis.line = element_line(colour = "black", size = 0.8),
        panel.grid.major = element_blank(),
        panel.grid.minor =element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(size = 15,face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 15,face = "bold"),
        axis.text.y = element_text(size = 12,face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)) +
  scale_y_continuous(limits = c(0, 800),
                     breaks = seq(0, 800,100),
                     expand = c(0,0))
 
# --------------------barplot: only for Structured > Random, 2 bars----------------------------

mean_sd_str <-subset (mean_sd_copy, Contrast == "Structured > Random")
mean_sd_str$Contrast <- c("Speech Structured > Random", "Tone Structured > Random")

# options(repr.plot.width = 1)

g_str = ggplot(mean_sd_str, aes(x = Contrast, y = `Mean of Top 10% Activity`)) +
  geom_bar(stat = "identity", position = "dodge", fill = "blue", width = 0.6) +
  geom_errorbar(aes(ymin = mean_sd_str$`Mean of Top 10% Activity` -se, ymax =mean_sd_str$`Mean of Top 10% Activity` +se),
                width = 0.1, size = 0.8, position = position_dodge(0.9)) +
  theme_bw() 

 g_str + theme(axis.line = element_line(colour = "black", size = 0.8),
          panel.grid.major = element_blank(),
          panel.grid.minor =element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title.x = element_text(size = 15,face = "bold"),
          axis.text.x = element_text(size = 12, face = "bold"),
          axis.title.y = element_text(size = 15,face = "bold"),
          axis.text.y = element_text(size = 12,face = "bold")
          ) +
  scale_y_continuous(limits = c(0, 800),
                     breaks = seq(0,800,100),
                     expand = c(0,0)) +
  scale_x_discrete(labels = c("Speech\nStructured > Random", "Tone\nStructured > Random")) +
   geom_text(x = 1, y = 780, label = "***", size = 8) +
   geom_text(x = 2, y = 780, label = "***", size = 8)


# png(filename = "left_cerebellum_activation", width = 780, height = 670)

## paired-sample test between ssl str-rand and tsl str-rand
t.test(leftCerebtop10$parcel2_ssl_str_rand,
       leftCerebtop10$parcel2_tsl_str_rand,
       alternative="two.sided",
       paired = TRUE,
       var.equal = FALSE
)

## one sample t test
t.test(leftCerebtop10$parcel2_ssl_str_rand,mu = 0,alternative = "two.sided")
t.test(leftCerebtop10$parcel2_tsl_str_rand,mu = 0,alternative = "two.sided")
# /Volumes/lendlab_raid/projects/sllip/make_parcels-tengwen/leftCerebellumtop10.R


#--------------------- scatter plot: only for Structured > Random -----------------------

## correlation
cor(leftCerebtop10$parcel2_ssl_str_rand, leftCerebtop10$parcel2_tsl_str_rand)
cor.test(leftCerebtop10$parcel2_ssl_str_rand, leftCerebtop10$parcel2_tsl_str_rand)

cerebellum_parcel_correlationplot <- ggscatter(leftCerebtop10, x = "parcel2_ssl_str_rand", y = "parcel2_tsl_str_rand",
                         add = "reg.line", conf.int = TRUE,
                         cor.coef = TRUE, cor.method = "pearson",
                         xlab = "parcel2_ssl_str_rand", ylab = "parcel2_tsl_str_rand") +
  scale_x_continuous(name="Speech\nStructured > Random") +
  scale_y_continuous(name="Tone\nStructured > Random") +
  theme(axis.title.x = element_text(size = 15,face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 15,face = "bold"),
        axis.text.y = element_text(size = 12,face = "bold"))

cerebellum_parcel_correlationplot


