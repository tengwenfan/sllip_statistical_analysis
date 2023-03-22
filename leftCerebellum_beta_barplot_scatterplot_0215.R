
# this script is for barplot and scatterplot of beta value
# data: beta value of nosmoothing data in cerebellum parcel2
# only strutured > random no smoothing data

# rm(list=ls())
library(dplyr);
library(tidyverse);
library(stats);
library(psych);
library(ggpubr);
library(tableone);
library("ggpubr");

# read beta values: group (conjunction/no-conjunction in cerebellum) * condition (speech/tone)  and organize data
cerebparcel2_sig_speech_str_rand <- read.csv("/Volumes/lendlab_raid/projects/sllip/make_parcels-tengwen/featquery/cerebellum_sig_speech_str_rand/cerebparcel2_sig_speech_str_rand.csv")
cerebparcel2_insig_speech_str_rand <- read.csv("/Volumes/lendlab_raid/projects/sllip/make_parcels-tengwen/featquery/cerebellum_insig_speech_str_rand/cerebparcel2_insig_speech_str_rand.csv")
cerebparcel2_sig_tone_str_rand <- read.csv("/Volumes/lendlab_raid/projects/sllip/make_parcels-tengwen/featquery/cerebellum_sig_tone_str_rand/cerebparcel2_sig_tone_str_rand.csv")
cerebparcel2_insig_tone_str_rand <- read.csv("/Volumes/lendlab_raid/projects/sllip/make_parcels-tengwen/featquery/cerebellum_insig_tone_str_rand/cerebparcel2_insig_tone_str_rand.csv")


cerebparcel2_sig_speech_str_rand_Mean <- subset(cerebparcel2_sig_speech_str_rand, select = c("stats_image_cerebparcel2_sig_speech_str_rand","mean_cerebparcel2_sig_speech_str_rand"))
cerebparcel2_insig_speech_str_rand_Mean <- subset(cerebparcel2_insig_speech_str_rand, select = c("stats_image_cerebparcel2_insig_speech_str_rand", "mean_cerebparcel2_insig_speech_str_rand"))
cerebparcel2_sig_tone_str_rand_Mean <- subset(cerebparcel2_sig_tone_str_rand, select = c("stats_image_cerebparcel2_sig_tone_str_rand", "mean_cerebparcel2_sig_tone_str_rand"))
cerebparcel2_insig_tone_str_rand_Mean <- subset(cerebparcel2_insig_tone_str_rand, select = c("stats_image_cerebparcel2_insig_tone_str_rand", "mean_cerebparcel2_insig_tone_str_rand"))


cerebellum_siggroup_subject_ID <- read.csv("/Volumes/lendlab_raid/projects/sllip/make_parcels-tengwen/cerebellum_siggroup_subject_ID.csv")
cerebellum_insiggroup_subject_ID <- read.csv("/Volumes/lendlab_raid/projects/sllip/make_parcels-tengwen/cerebellum_insiggroup_subject_ID.csv")

cerebparcel2_sig_speech_str_rand_Mean <- cbind(cerebellum_siggroup_subject_ID, cerebparcel2_sig_speech_str_rand_Mean)
cerebparcel2_insig_speech_str_rand_Mean <- cbind(cerebellum_insiggroup_subject_ID, cerebparcel2_insig_speech_str_rand_Mean)
cerebparcel2_sig_tone_str_rand_Mean <- cbind(cerebellum_siggroup_subject_ID, cerebparcel2_sig_tone_str_rand_Mean)
cerebparcel2_insig_tone_str_rand_Mean <- cbind(cerebellum_insiggroup_subject_ID, cerebparcel2_insig_tone_str_rand_Mean)

cerebparcel2_sig_speech_str_rand_Mean <- cerebparcel2_sig_speech_str_rand_Mean[cerebparcel2_sig_speech_str_rand_Mean$stats_image_cerebparcel2_sig_speech_str_rand == "stats/zstat2", ]
cerebparcel2_insig_speech_str_rand_Mean <- cerebparcel2_insig_speech_str_rand_Mean[cerebparcel2_insig_speech_str_rand_Mean$stats_image_cerebparcel2_insig_speech_str_rand == "stats/zstat2", ]
cerebparcel2_sig_tone_str_rand_Mean <- cerebparcel2_sig_tone_str_rand_Mean[cerebparcel2_sig_tone_str_rand_Mean$stats_image_cerebparcel2_sig_tone_str_rand == "stats/zstat1", ]
cerebparcel2_insig_tone_str_rand_Mean <- cerebparcel2_insig_tone_str_rand_Mean[cerebparcel2_insig_tone_str_rand_Mean$stats_image_cerebparcel2_insig_tone_str_rand == "stats/zstat1", ]


# write.csv(cerebparcel2_sig_speech_str_rand_Mean, "cerebparcel2_sig_speech_str_rand_Mean.csv")
# write.csv(cerebparcel2_insig_speech_str_rand_Mean, "cerebparcel2_insig_speech_str_rand_Mean.csv")

colnames(cerebparcel2_sig_speech_str_rand_Mean)[4] <- "stats_image_cerebparcel2_speech_str_rand"
colnames(cerebparcel2_insig_speech_str_rand_Mean)[4] <- "stats_image_cerebparcel2_speech_str_rand"
colnames(cerebparcel2_sig_tone_str_rand_Mean)[4] <- "stats_image_cerebparcel2_tone_str_rand"
colnames(cerebparcel2_insig_tone_str_rand_Mean)[4] <- "stats_image_cerebparcel2_tone_str_rand"

colnames(cerebparcel2_sig_speech_str_rand_Mean)[5] <- "mean_cerebparcel2_speech_str_rand"
colnames(cerebparcel2_insig_speech_str_rand_Mean)[5] <- "mean_cerebparcel2_speech_str_rand"
colnames(cerebparcel2_sig_tone_str_rand_Mean)[5] <- "mean_cerebparcel2_tone_str_rand"
colnames(cerebparcel2_insig_tone_str_rand_Mean)[5] <- "mean_cerebparcel2_tone_str_rand"


cerebparcel2_speech_str_rand <- rbind(cerebparcel2_sig_speech_str_rand_Mean, cerebparcel2_insig_speech_str_rand_Mean)
cerebparcel2_tone_str_rand <- rbind(cerebparcel2_sig_tone_str_rand_Mean, cerebparcel2_insig_tone_str_rand_Mean)


write.csv(cerebparcel2_speech_str_rand, "cerebparcel2_speech_str_rand.csv")
write.csv(cerebparcel2_tone_str_rand, "cerebparcel2_tone_str_rand.csv")


cerebparcel2_speech_str_rand <- read.csv("cerebparcel2_speech_str_rand.csv")
cerebparcel2_tone_str_rand <- read.csv("cerebparcel2_tone_str_rand.csv")

cerebparcel2_speech_str_rand <- cerebparcel2_speech_str_rand[ ,-c(1)]
cerebparcel2_tone_str_rand <- cerebparcel2_tone_str_rand[ ,-c(1)]


cerebparcel2_str_rand <- merge (cerebparcel2_speech_str_rand,cerebparcel2_tone_str_rand,by='subject_ID',all=TRUE,sort=TRUE, suffixes = c("_speech","_tone"))

cerebparcel2_str_rand <- subset(cerebparcel2_str_rand, select = -c(subject_ID_sllip_tone,group_tone) )
colnames(cerebparcel2_str_rand)[which(names(cerebparcel2_str_rand) == "subject_ID_sllip_speech")] <- "subject_ID_sllip"
colnames(cerebparcel2_str_rand)[which(names(cerebparcel2_str_rand) == "group_speech")] <- "group"

# this is the final organized beta data!!!!!
cerebparcel2_str_rand <- subset(cerebparcel2_str_rand, select = c(subject_ID,group, mean_cerebparcel2_speech_str_rand,mean_cerebparcel2_tone_str_rand) )


# ---------------------------------------------pivot_longer data -----------------
cerebparcel2_str_rand_long <- pivot_longer(
  cerebparcel2_str_rand,
  cols = c(mean_cerebparcel2_speech_str_rand, mean_cerebparcel2_tone_str_rand),
  names_to = "condition",
  values_to = "beta_activation",
  names_transform = list(condition = as.factor)) %>%
  mutate(group = as.factor(group))

# ----------anova------------
m_cerebparcel2_str_rand_long <- aov(beta_activation ~ group*condition, data = cerebparcel2_str_rand_long)
m_cerebparcel2_str_rand_long
summary(m_cerebparcel2_str_rand_long)


# --------barplot: group (conjunction/no-conjunction in cerebellum) * condition (speech/tone) ----------------
mean_sd_cerebparcel2_str_rand_long <- cerebparcel2_str_rand_long %>%
  group_by(group,condition) %>%
  summarize(mean_beta_activation = mean(beta_activation),
            sd = sd(beta_activation),
            se = sd(beta_activation)/sqrt(n()))

mean_sd_cerebparcel2_str_rand_long$Group <- c("No Conjunction", "No Conjunction","Conjunction", "Conjunction")
mean_sd_cerebparcel2_str_rand_long$Task <- c("Speech", "Tone", "Speech", "Tone")

colnames(mean_sd_cerebparcel2_str_rand_long)[which(names(mean_sd_cerebparcel2_str_rand_long) == "mean_beta_activation")] <- "Beta Values"


g = ggplot(mean_sd_cerebparcel2_str_rand_long, aes(x = Task, y = `Beta Values`, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_sd_cerebparcel2_str_rand_long$`Beta Values` -se, ymax =mean_sd_cerebparcel2_str_rand_long$`Beta Values` +se),
                width = 0.2, size = 0.8, position = position_dodge(0.9)) +
  theme_bw() +
  theme(legend.position = c(.8, .92))

g

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
  #scale_y_continuous(limits = c(-.15, .35),
   #                  breaks = seq(-.15, 0.1,0.35),
   #                  expand = c(0,0)) +
  scale_x_discrete(labels = c("Speech\nStructured > Random", "Tone\nStructured > Random"))


# ------------------------------ t test----------------------------
# on sample t test
# data of each group for each condition 
no_smoothed_conjunction_ssl_str_rand_beta <- subset(cerebparcel2_str_rand_long, group == "sig" & condition == "mean_cerebparcel2_speech_str_rand")
no_smoothed_conjunction_tsl_str_rand_beta <- subset(cerebparcel2_str_rand_long, group == "sig" & condition == "mean_cerebparcel2_tone_str_rand")
no_smoothed_noconjunction_ssl_str_rand_beta <- subset(cerebparcel2_str_rand_long, group == "insig" & condition == "mean_cerebparcel2_speech_str_rand")
no_smoothed_noconjunction_tsl_str_rand_beta <- subset(cerebparcel2_str_rand_long, group == "insig" & condition == "mean_cerebparcel2_tone_str_rand")

# data of each condition
no_smoothed_ssl_str_rand_beta <- subset(cerebparcel2_str_rand_long, condition == "mean_cerebparcel2_speech_str_rand")
no_smoothed_tsl_str_rand_beta <- subset(cerebparcel2_str_rand_long, condition == "mean_cerebparcel2_tone_str_rand")

## one sample t test for each condition
t.test(no_smoothed_ssl_str_rand_beta$beta_activation,mu = 0,alternative = "two.sided")
t.test(no_smoothed_tsl_str_rand_beta$beta_activation,mu = 0,alternative = "two.sided")


# ------------------------ scatter plot: structure > random-----------------------------
cor.test(no_smoothed_ssl_str_rand_beta$beta_activation, no_smoothed_tsl_str_rand_beta$beta_activation)

cerebellum_parcel_correlationplot_beta <- ggscatter(cerebparcel2_str_rand, x = "mean_cerebparcel2_speech_str_rand", y = "mean_cerebparcel2_tone_str_rand",
                                               add = "reg.line", conf.int = TRUE,
                                               cor.coef = TRUE, cor.method = "pearson",
                                               xlab = "parcel2_ssl_str_rand_beta", ylab = "parcel2_tsl_str_rand_beta") +
  scale_x_continuous(name="Speech\nStructured > Random") +
  scale_y_continuous(name="Tone\nStructured > Random") +
  theme(axis.title.x = element_text(size = 15,face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 15,face = "bold"),
        axis.text.y = element_text(size = 12,face = "bold"))

cerebellum_parcel_correlationplot_beta



## ----------------------------- condition * group correlation figure -------------------
cerebparcel2_str_rand_cor_group <-ggscatter(cerebparcel2_str_rand, x = "mean_cerebparcel2_speech_str_rand", y = "mean_cerebparcel2_tone_str_rand",
                                    color = "group", size = 4, #shape = "sig",
                                    add = "reg.line", conf.int = TRUE,
                                    # cor.coef = TRUE, cor.method = "pearson",
                                    xlab = "mean_cerebparcel2_speech_str_rand", ylab = "mean_cerebparcel2_tone_str_rand") +
  stat_cor(aes(color = group, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), show.legend = FALSE, size = 5) +
  scale_color_discrete(
    name = "Group",
    labels = c("No Conjunction", "Conjunction")) +
  scale_fill_discrete(
    name = "Group",
    labels = c("No Conjunction", "Conjunction")) +
  scale_x_continuous(name="Speech\nStructured > Random") +
  scale_y_continuous(name="Tone\nStructured > Random") +
  theme(axis.title.x = element_text(size = 15,face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 15,face = "bold"),
        axis.text.y = element_text(size = 12,face = "bold"))