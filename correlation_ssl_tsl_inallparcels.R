

# This is the script for statistical analysis of correlation between speech and tone in each parcels (both correlation r values with froi data and lpsa r values)


# rm(list=ls())
library(dplyr);
library(tidyverse);
library(stats);
library(psych);
library(ggpubr);
library(tableone);
library("ggpubr");

#    ------------------------------------------------correlation between speech and tone in each SST and TSL structured > random parcels, with froi values --------------
##--SSL_structure_rand_parcel vox3------------
ssl_str_rd_parcel_ssl_str_rand_froi <- read.csv("/Volumes/lendlab_raid/projects/sllip/make_parcels-tengwen/data/froi_analysis_ssl_str-rd_vox3_parcel_SSL_str-rand_0228/stat_learning_contrast_ssl_str-rd_vox3_parcel_SSL_str-rand_vox3_0228_froi_resp_mag.csv",header = TRUE)
ssl_str_rd_parcel_tsl_str_rand_froi <- read.csv("/Volumes/lendlab_raid/projects/sllip/make_parcels-tengwen/data/froi_analysis_ssl_str-rd_vox3_parcel_tsl_str-rand_0228/stat_learning_contrast_ssl_str-rd_vox3_parcel_tsl_str-rand_0228_froi_resp_mag.csv",header = TRUE)
# parcel 1 3 5 8 13

colnames(ssl_str_rd_parcel_ssl_str_rand_froi)[1] <- "ssl_parcel_1_ssl_str_rand"
colnames(ssl_str_rd_parcel_ssl_str_rand_froi)[2] <- "ssl_parcel_3_ssl_str_rand"
colnames(ssl_str_rd_parcel_ssl_str_rand_froi)[3] <- "ssl_parcel_5_ssl_str_rand"
colnames(ssl_str_rd_parcel_ssl_str_rand_froi)[4] <- "ssl_parcel_8_ssl_str_rand"
colnames(ssl_str_rd_parcel_ssl_str_rand_froi)[5] <- "ssl_parcel_13_ssl_str_rand"

colnames(ssl_str_rd_parcel_tsl_str_rand_froi)[1] <- "ssl_parcel_1_tsl_str_rand"
colnames(ssl_str_rd_parcel_tsl_str_rand_froi)[2] <- "ssl_parcel_3_tsl_str_rand"
colnames(ssl_str_rd_parcel_tsl_str_rand_froi)[3] <- "ssl_parcel_5_tsl_str_rand"
colnames(ssl_str_rd_parcel_tsl_str_rand_froi)[4] <- "ssl_parcel_8_tsl_str_rand"
colnames(ssl_str_rd_parcel_tsl_str_rand_froi)[5] <- "ssl_parcel_13_tsl_str_rand"

ssl_parcel_froi <-cbind(ssl_str_rd_parcel_ssl_str_rand_froi, ssl_str_rd_parcel_tsl_str_rand_froi)

cor.test(ssl_str_rd_parcel_ssl_str_rand_froi$ssl_parcel_1_ssl_str_rand, ssl_str_rd_parcel_tsl_str_rand_froi$ssl_parcel_1_tsl_str_rand)
# corr.test(ssl_str_rd_parcel_ssl_str_rand_froi$parcel_1, ssl_str_rd_parcel_tsl_str_rand_froi$parcel_1)
# cor(ssl_str_rd_parcel_ssl_str_rand_froi$parcel_1, ssl_str_rd_parcel_tsl_str_rand_froi$parcel_1)


ssl_parcel1 <- ggscatter(ssl_parcel_froi, x = "ssl_parcel_1_ssl_str_rand", y = "ssl_parcel_1_tsl_str_rand",
                         add = "reg.line", conf.int = TRUE,
                         cor.coef = TRUE, cor.method = "pearson",
                         xlab = "ssl_parcel_1_ssl_str_rand", ylab = "ssl_parcel_1_tsl_str_rand")
# ssl_parcel1
ssl_parcel3 <- ggscatter(ssl_parcel_froi, x = "ssl_parcel_3_ssl_str_rand", y = "ssl_parcel_3_tsl_str_rand",
                         add = "reg.line", conf.int = TRUE,
                         cor.coef = TRUE, cor.method = "pearson",
                         xlab = "ssl_parcel_3_ssl_str_rand", ylab = "ssl_parcel_3_tsl_str_rand")

ssl_parcel5 <- ggscatter(ssl_parcel_froi, x = "ssl_parcel_5_ssl_str_rand", y = "ssl_parcel_5_tsl_str_rand",
                         add = "reg.line", conf.int = TRUE,
                         cor.coef = TRUE, cor.method = "pearson",
                         xlab = "ssl_parcel_5_ssl_str_rand", ylab = "ssl_parcel_5_tsl_str_rand")

ssl_parcel8 <- ggscatter(ssl_parcel_froi, x = "ssl_parcel_8_ssl_str_rand", y = "ssl_parcel_8_tsl_str_rand",
                         add = "reg.line", conf.int = TRUE,
                         cor.coef = TRUE, cor.method = "pearson",
                         xlab = "ssl_parcel_8_ssl_str_rand", ylab = "ssl_parcel_8_tsl_str_rand")

ssl_parcel13 <- ggscatter(ssl_parcel_froi, x = "ssl_parcel_13_ssl_str_rand", y = "ssl_parcel_13_tsl_str_rand",
                         add = "reg.line", conf.int = TRUE,
                         cor.coef = TRUE, cor.method = "pearson",   # cor.coef = FALSE
                         xlab = "ssl_parcel_13_ssl_str_rand", ylab = "ssl_parcel_13_tsl_str_rand")


ggarrange(ssl_parcel1, ssl_parcel3, ssl_parcel5, ssl_parcel8, ssl_parcel13 + rremove("x.text"), ncol = 3, nrow = 2)



ssl_parcel13 +
  stat_cor(method = "pearson", size = 6, label.y = -100) +
  scale_x_continuous(name="Speech\nStructured > Random") +
  scale_y_continuous(name="Tone\nStructured > Random") +
  theme(axis.title.x = element_text(size = 15,face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 15,face = "bold"),
        axis.text.y = element_text(size = 12,face = "bold"))




### -----tsl_structure_rand_parcel

tsl_str_rd_parcel_ssl_str_rand_froi <- read.csv("/Volumes/lendlab_raid/projects/sllip/make_parcels-tengwen/data/froi_analysis_tsl_str-rd_vox3_parcel_SSL_str-rand_0228/stat_learning_contrast_tsl_str-rd_vox3_parcel_SSL_str-rand_0228_froi_resp_mag.csv",header = TRUE)
tsl_str_rd_parcel_tsl_str_rand_froi <- read.csv("/Volumes/lendlab_raid/projects/sllip/make_parcels-tengwen/data/froi_analysis_tsl_str-rd_vox3_parcel_tsl_str-rand_0228/stat_learning_contrast_tsl_str-rd_vox3_parcel_tsl_str-rand_0228_froi_resp_mag.csv",header = TRUE)
# parcel 5 7

colnames(tsl_str_rd_parcel_ssl_str_rand_froi)[1] <- "tsl_parcel_5_ssl_str_rand"
colnames(tsl_str_rd_parcel_ssl_str_rand_froi)[2] <- "tsl_parcel_7_ssl_str_rand"

colnames(tsl_str_rd_parcel_tsl_str_rand_froi)[1] <- "tsl_parcel_5_tsl_str_rand"
colnames(tsl_str_rd_parcel_tsl_str_rand_froi)[2] <- "tsl_parcel_7_tsl_str_rand"

tsl_parcel_froi <-cbind(tsl_str_rd_parcel_ssl_str_rand_froi, tsl_str_rd_parcel_tsl_str_rand_froi)

tsl_parcel5 <- ggscatter(tsl_parcel_froi, x = "tsl_parcel_5_ssl_str_rand", y = "tsl_parcel_5_tsl_str_rand",
                         add = "reg.line", conf.int = TRUE,
                         cor.coef = TRUE, cor.method = "pearson", # cor.coef = FALSE
                         xlab = "tsl_parcel_5_ssl_str_rand", ylab = "tsl_parcel_5_tsl_str_rand")


tsl_parcel7 <- ggscatter(tsl_parcel_froi, x = "tsl_parcel_7_ssl_str_rand", y = "tsl_parcel_7_tsl_str_rand",
                         add = "reg.line", conf.int = TRUE,
                         cor.coef = TRUE, cor.method = "pearson",
                         xlab = "tsl_parcel_7_ssl_str_rand", ylab = "tsl_parcel_7_tsl_str_rand")


ggarrange(tsl_parcel5, tsl_parcel7, ncol = 2, nrow = 1)



tsl_parcel5 +
stat_cor(method = "pearson", size = 6, label.y = -50) +
  scale_x_continuous(name="Speech\nStructured > Random") +
  scale_y_continuous(name="Tone\nStructured > Random") +
  theme(axis.title.x = element_text(size = 15,face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 15,face = "bold"),
        axis.text.y = element_text(size = 12,face = "bold"))




# ------------------------------------------------t tests of lpsa correlation r value between speech and tone in each SST and TSL structured > random parcels --------------
## ------------------------
# lpsa analysis: correlation between speech and tone within ssl str > rand parcels
rval_ssl_str_rand_5parcels <- read.csv('/Volumes/lendlab_raid/projects/sllip/make_parcels-tengwen/data/corr_analysis_results_ssl_str_rd_vox3/rval_in_parcel.csv')

t.test(rval_ssl_str_rand_5parcels$parcel_1,mu = 0,alternative = "two.sided")
t.test(rval_ssl_str_rand_5parcels$parcel_3,mu = 0,alternative = "two.sided")
t.test(rval_ssl_str_rand_5parcels$parcel_5,mu = 0,alternative = "two.sided")
t.test(rval_ssl_str_rand_5parcels$parcel_8,mu = 0,alternative = "two.sided")
t.test(rval_ssl_str_rand_5parcels$parcel_13,mu = 0,alternative = "two.sided")

# lpsa analysis: correlation between speech and tone within tsl str > rand parcels
rval_tsl_str_rand_5parcels <- read.csv('/Volumes/lendlab_raid/projects/sllip/make_parcels-tengwen/data/corr_analysis_results_tsl_str_rd_vox3/rval_in_parcel.csv')

t.test(rval_tsl_str_rand_5parcels$parcel_5,mu = 0,alternative = "two.sided")
t.test(rval_tsl_str_rand_5parcels$parcel_7,mu = 0,alternative = "two.sided")


# lpsa analysis: correlation between speech and tone within ssl str > rand parcels, no smoothing data
rval_ssl_str_rand_5parcels_nosmoothing <- read.csv('/Volumes/lendlab_raid/projects/sllip/make_parcels-tengwen/data/corr_analysis_results_ssl_str_rd_vox3_nosmoothing/rval_in_parcel.csv')

t.test(rval_ssl_str_rand_5parcels_nosmoothing$parcel_1,mu = 0,alternative = "two.sided")
t.test(rval_ssl_str_rand_5parcels_nosmoothing$parcel_3,mu = 0,alternative = "two.sided")
t.test(rval_ssl_str_rand_5parcels_nosmoothing$parcel_5,mu = 0,alternative = "two.sided")
t.test(rval_ssl_str_rand_5parcels_nosmoothing$parcel_8,mu = 0,alternative = "two.sided")
t.test(rval_ssl_str_rand_5parcels_nosmoothing$parcel_13,mu = 0,alternative = "two.sided")


# lpsa analysis: correlation between speech and tone within tsl str > rand parcels, no smoothing data
rval_tsl_str_rand_5parcels_nosmoothing <- read.csv('/Volumes/lendlab_raid/projects/sllip/make_parcels-tengwen/data/corr_analysis_results_tsl_str_rd_vox3_nosmoothing/rval_in_parcel.csv')

t.test(rval_tsl_str_rand_5parcels_nosmoothing$parcel_5,mu = 0,alternative = "two.sided")
t.test(rval_tsl_str_rand_5parcels_nosmoothing$parcel_7,mu = 0,alternative = "two.sided")



## boxplot for ssl rval
rval_ssl_str_rand_5parcels_nosmoothing <- cbind(subject_ID, rval_ssl_str_rand_5parcels_nosmoothing)

rval_ssl_str_rand_5parcels_nosmoothing_long <- pivot_longer(
  rval_ssl_str_rand_5parcels_nosmoothing,
  cols = c(parcel_1, parcel_3, parcel_5, parcel_8, parcel_13),
  names_to = "Parcel",
  values_to = "rval",
  names_transform = list(Parcel = as.factor)) %>%
  mutate(subject_ID = as.factor(subject_ID)
  )


rval_ssl_str_rand_5parcels_nosmoothing_long$ParcelSeq <- with(rval_ssl_str_rand_5parcels_nosmoothing_long, ifelse(Parcel == 'parcel_1', 'parcel01',
                                                                                              ifelse(Parcel == 'parcel_3', 'parcel03',
                                                                                                     ifelse(Parcel == 'parcel_5', 'parcel05',
                                                                                                            ifelse(Parcel == 'parcel_8', 'parcel08','parcel13')))))
# this plotSeq variable is to order the boxplot consistent with the color order in glass brain plots
rval_ssl_str_rand_5parcels_nosmoothing_long$plotSeq <- with(rval_ssl_str_rand_5parcels_nosmoothing_long, ifelse(Parcel == 'parcel_1', 'plot3',
                                                                                                                  ifelse(Parcel == 'parcel_3', 'plot1',
                                                                                                                         ifelse(Parcel == 'parcel_5', 'plot4',
                                                                                                                                ifelse(Parcel == 'parcel_8', 'plot5','plot2')))))


g_ssl_rval <- ggplot(rval_ssl_str_rand_5parcels_nosmoothing_long, aes(plotSeq, rval, fill = plotSeq)) +
  geom_boxplot() + 
  geom_jitter(width = 0.15)
#  scale_color_manual(values = c("green","yellow","red","purple","black"))  #name = "ParcelSeq",

g_ssl_rval

g_ssl_rval + theme(axis.line = element_line(colour = "black", size = 0.8),
      panel.grid.major = element_blank(),
      panel.grid.minor =element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.title.x = element_blank(),
      #axis.title.x = element_text(size = 15,face = "bold"),
      axis.text.x = element_text(size = 12, face = "bold", angle = 15, hjust = 0.9),
      axis.title.y = element_text(size = 15,face = "bold"),
      axis.text.y = element_text(size = 12,face = "bold"),
      legend.position = "none")+
  labs(y = expression('Correlation Values ('~italic(R)~')')) +  #, x = "Parcel"
  scale_x_discrete(labels = c("Left STG/\nAngular Gyrus", "Right STG", "Right Cerebellum", "Right Frontal Pole", "Right IFG"))+
  scale_y_continuous(limits = c(-1, 0.5),
                     breaks = seq(-1, 0.5,0.5),
                     expand = c(0,0)) +
  scale_fill_manual(values = c("#F46D43","#5E4FA2","#9E0142","#FEE08B","#E6F598")) +
  geom_text(x = 1, y = .47, label = "***", size = 6, family = "Times") +
  geom_text(x = 2, y = .47, label = "***", size = 6, family = "Times") +
  geom_text(x = 3, y = .47, label = "***", size = 6, family = "Times") +
  geom_text(x = 4, y = .47, label = "***", size = 6, family = "Times") +
  geom_text(x = 5, y = .47, label = "***", size = 6, family = "Times") 
  



## boxplot for tsl rval----------------------------------------
rval_tsl_str_rand_5parcels_nosmoothing <- cbind(subject_ID, rval_tsl_str_rand_5parcels_nosmoothing)

rval_tsl_str_rand_5parcels_nosmoothing_long <- pivot_longer(
  rval_tsl_str_rand_5parcels_nosmoothing,
  cols = c(parcel_5, parcel_7),
  names_to = "Parcel",
  values_to = "rval",
  names_transform = list(Parcel = as.factor)) %>%
  mutate(subject_ID = as.factor(subject_ID)
  )

rval_tsl_str_rand_5parcels_nosmoothing_long$ParcelSeq <- with(rval_tsl_str_rand_5parcels_nosmoothing_long, ifelse(Parcel == 'parcel_5', 'parcel05','parcel07'))
                                                                                                                  

g_tsl_rval <- ggplot(rval_tsl_str_rand_5parcels_nosmoothing_long, aes(ParcelSeq, rval, fill = ParcelSeq)) +
  geom_boxplot() + 
  geom_jitter(width = 0.15) #+
#  scale_color_manual(values = c("green","yellow","red","purple","black"))  #name = "ParcelSeq",

g_tsl_rval

g_tsl_rval + theme(axis.line = element_line(colour = "black", size = 0.8),
                   panel.grid.major = element_blank(),
                   panel.grid.minor =element_blank(),
                   panel.border = element_blank(),
                   panel.background = element_blank(),
                   axis.title.x = element_blank(), #size = 15,face = "bold"
                   axis.text.x = element_text(size = 12, face = "bold", angle = 15, hjust = 0.9),
                   axis.title.y = element_text(size = 15,face = "bold"),
                   axis.text.y = element_text(size = 12,face = "bold"),
                   legend.position = "none") +
  labs(y = expression('Correlation Values ('~italic(R)~')')) +  #, x = "Parcel"
  scale_x_discrete(labels = c("Cingulate Gyrus", "Left Frontal Pole"))+
  scale_y_continuous(limits = c(-1, 0.5),
                     breaks = seq(-1, 0.5,0.5),
                     expand = c(0,0)) +
  scale_fill_manual(values = c("#008080","#FF8080")) +
  geom_text(x = 1, y = .47, label = "***", size = 6, family = "Times") +
  geom_text(x = 2, y = .47, label = "***", size = 6, family = "Times")






# ------------------------------------------------correlation between speech and tone in SST random > structured parcels, with froi values --------------
### ----------SSL_rand_structure_parcel--
ssl_rd_str_parcel_ssl_str_rand_froi <- read.csv("/Volumes/lendlab_raid/projects/sllip/make_parcels-tengwen/data/froi_analysis_ssl_rd-str_parcel_SSL_str-rand_0208/stat_learning_contrast_ssl_rd-str_parcel_SSL_str-rand_0208_froi_resp_mag.csv",header = TRUE)
ssl_rd_str_parcel_tsl_str_rand_froi <- read.csv("/Volumes/lendlab_raid/projects/sllip/make_parcels-tengwen/data/froi_analysis_ssl_rd-str_parcel_tsl_str-rand_0208/stat_learning_contrast_ssl_rd-str_parcel_tsl_str-rand_0208_froi_resp_mag.csv",header = TRUE)
# parcel 7,39,59

colnames(ssl_rd_str_parcel_ssl_str_rand_froi)[1] <- "ssl_rand_parcel_7_ssl_str_rand"
colnames(ssl_rd_str_parcel_ssl_str_rand_froi)[2] <- "ssl_rand_parcel_39_ssl_str_rand"
colnames(ssl_rd_str_parcel_ssl_str_rand_froi)[3] <- "ssl_rand_parcel_59_ssl_str_rand"

colnames(ssl_rd_str_parcel_tsl_str_rand_froi)[1] <- "ssl_rand_parcel_7_tsl_str_rand"
colnames(ssl_rd_str_parcel_tsl_str_rand_froi)[2] <- "ssl_rand_parcel_39_tsl_str_rand"
colnames(ssl_rd_str_parcel_tsl_str_rand_froi)[3] <- "ssl_rand_parcel_59_tsl_str_rand"

ssl_rand_parcel_froi <-cbind(ssl_rd_str_parcel_ssl_str_rand_froi, ssl_rd_str_parcel_tsl_str_rand_froi)

ssl_rand_parcel7 <- ggscatter(ssl_rand_parcel_froi, x = "ssl_rand_parcel_7_ssl_str_rand", y = "ssl_rand_parcel_7_tsl_str_rand",
                         add = "reg.line", conf.int = TRUE,
                         cor.coef = TRUE, cor.method = "pearson",
                         xlab = "ssl_rand_parcel_7_ssl_str_rand", ylab = "ssl_rand_parcel_7_tsl_str_rand")

ssl_rand_parcel39 <- ggscatter(ssl_rand_parcel_froi, x = "ssl_rand_parcel_39_ssl_str_rand", y = "ssl_rand_parcel_39_tsl_str_rand",
                              add = "reg.line", conf.int = TRUE,
                              cor.coef = TRUE, cor.method = "pearson",
                              xlab = "ssl_rand_parcel_39_ssl_str_rand", ylab = "ssl_rand_parcel_39_tsl_str_rand")

ssl_rand_parcel59 <- ggscatter(ssl_rand_parcel_froi, x = "ssl_rand_parcel_59_ssl_str_rand", y = "ssl_rand_parcel_59_tsl_str_rand",
                               add = "reg.line", conf.int = TRUE,
                               cor.coef = TRUE, cor.method = "pearson",
                               xlab = "ssl_rand_parcel_59_ssl_str_rand", ylab = "ssl_rand_parcel_59_tsl_str_rand")


ggarrange(ssl_rand_parcel7, ssl_rand_parcel39, ssl_rand_parcel59, ncol = 3, nrow = 1)





## lpsa analysis: correlation between speech and tone within cerebellum parcel2, no smoothing data

rval_cerebellum_parcel2_nosmoothing <- read.csv('/Volumes/lendlab_raid/projects/sllip/make_parcels-tengwen/data/corr_analysis_results_cerebellumparcel2_vox3_nosmoothing_0305/rval_in_parcel.csv')

t.test(rval_cerebellum_parcel2_nosmoothing$parcel_2,mu = 0,alternative = "two.sided")



## boxplot for cerebellum----------------------------------------
rval_cerebellum_parcel2_nosmoothing <- cbind(subject_ID, rval_cerebellum_parcel2_nosmoothing)

rval_cerebellum_parcel2_nosmoothing_long <- pivot_longer(
  rval_cerebellum_parcel2_nosmoothing,
  cols = c(parcel_2),
  names_to = "Parcel",
  values_to = "rval",
  names_transform = list(Parcel = as.factor)) %>%
  mutate(subject_ID = as.factor(subject_ID)
  )


g_cereb_rval <- ggplot(rval_cerebellum_parcel2_nosmoothing_long, aes(Parcel, rval, fill = Parcel)) +
  geom_boxplot() + 
  geom_jitter(width = 0.15) #+
#  scale_color_manual(values = c("green","yellow","red","purple","black"))  #name = "ParcelSeq",

g_cereb_rval

g_cereb_rval + theme(axis.line = element_line(colour = "black", size = 0.8),
                   panel.grid.major = element_blank(),
                   panel.grid.minor =element_blank(),
                   panel.border = element_blank(),
                   panel.background = element_blank(),
                   axis.title.x = element_blank(), #size = 15,face = "bold"
                   axis.text.x = element_text(size = 12, face = "bold"), #, angle = 0, hjust = 0
                   axis.title.y = element_text(size = 15,face = "bold"),
                   axis.text.y = element_text(size = 12,face = "bold"),
                   legend.position = "none") +
  labs(y = expression('Correlation Values ('~italic(R)~')')) +  #, x = "Parcel"
  scale_x_discrete(labels = c("Cerebellum"))+
  scale_y_continuous(limits = c(-1, 0.7),
                     breaks = seq(-1, 0.7,0.5),
                     expand = c(0,0)) +
  scale_fill_manual(values = c("#E0C2F2")) +
  geom_text(y = 0.65, label = "*", size = 6, family = "Times")  



## boxplot for cerebellum, two groups----------------------------------------
rval_cerebellum_parcel2_nosmoothing <- read.csv('/Volumes/lendlab_raid/projects/sllip/make_parcels-tengwen/data/corr_analysis_results_cerebellumparcel2_vox3_nosmoothing_0305/rval_in_parcel.csv')
subject_ID_conjGroup <- read.csv('/Volumes/lendlab_raid/projects/sllip/make_parcels-tengwen/cerebellum_group_subject_ID.csv')

rval_cerebellum_parcel2_nosmoothing_conjGroup <- cbind(subject_ID_conjGroup, rval_cerebellum_parcel2_nosmoothing)


rval_cerebellum_parcel2_nosmoothing_conjGroup_long <- pivot_longer(
  rval_cerebellum_parcel2_nosmoothing_conjGroup,
  cols = c(parcel_2),
  names_to = "Parcel",
  values_to = "rval",
  names_transform = list(Parcel = as.factor)) %>%
  mutate(subject_ID = as.factor(subject_ID)
  )


# aes(color=factor(wt))

g_cereb_rval_conjGroup <- ggplot(rval_cerebellum_parcel2_nosmoothing_conjGroup_long, aes(Parcel, rval, fill = Parcel)) +
  geom_boxplot() +
  geom_point(position=position_jitterdodge(jitter.width=0.15, dodge.width = 0.3), 
             aes(color=group), show.legend = TRUE, size  = 2.5) +
  scale_fill_manual(values = c("#E0C2F2"), guide = "none") +
  scale_color_manual(values = c("sig" = "#0000FF", "insig" = "red"), labels = c('Conjunction','No Conjunction')) # black & red ; #00CED1;#0F9EA1
  

g_cereb_rval_conjGroup + theme(axis.line = element_line(colour = "black", size = 0.8),
                               panel.grid.major = element_blank(),
                               panel.grid.minor =element_blank(),
                               panel.border = element_blank(),
                               panel.background = element_blank(),
                               axis.title.x = element_blank(), #size = 15,face = "bold"
                               axis.text.x = element_text(size = 12, face = "bold"), #, angle = 0, hjust = 0
                               axis.title.y = element_text(size = 14,face = "bold"),
                               axis.text.y = element_text(size = 12,face = "bold"),
                               ) + # legend.position = "none"
  labs(y = expression('Correlation Values ('~italic(R)~')')) +  #, x = "Parcel"
  scale_x_discrete(labels = c("Cerebellum"))+
  scale_y_continuous(limits = c(-1, 0.7),
                     breaks = seq(-1, 0.7,0.5),
                     expand = c(0,0)) +
#  scale_fill_manual(values = c("gray")) +
  geom_text(y = 0.65, label = "*", size = 6, family = "Times")  +
  theme(#legend.position = c(.85,.1),
        legend.text=element_text(size=10),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white"),
        #legend.key.size = unit(0.8, 'cm'),
        legend.key=element_blank(),
        legend.position='bottom',
        legend.spacing.x = unit(0, 'cm'),
        legend.margin=margin(-10,0,0,0))
  



t.test(rval_cerebellum_parcel2_nosmoothing_conjGroup_long$rval[rval_cerebellum_parcel2_nosmoothing_conjGroup_long$group == "sig"],
       rval_cerebellum_parcel2_nosmoothing_conjGroup_long$rval[rval_cerebellum_parcel2_nosmoothing_conjGroup_long$group == "insig"],
       alternative="two.sided",
       paired = TRUE,
       var.equal = FALSE
)


mean(rval_cerebellum_parcel2_nosmoothing_conjGroup_long$rval[rval_cerebellum_parcel2_nosmoothing_conjGroup_long$group == "sig"])
mean(rval_cerebellum_parcel2_nosmoothing_conjGroup_long$rval[rval_cerebellum_parcel2_nosmoothing_conjGroup_long$group == "insig"])

t.test(rval_cerebellum_parcel2_nosmoothing_conjGroup_long$rval[rval_cerebellum_parcel2_nosmoothing_conjGroup_long$group == "sig"], mu = 0,alternative = "two.sided")
t.test(rval_cerebellum_parcel2_nosmoothing_conjGroup_long$rval[rval_cerebellum_parcel2_nosmoothing_conjGroup_long$group == "insig"], mu = 0,alternative = "two.sided")


#  boxplot for cerebellum, boxes for two groups-------------

g_cereb_rval_conjGroup_box <- ggplot(rval_cerebellum_parcel2_nosmoothing_conjGroup_long, aes(group, rval, fill = group)) +
  geom_boxplot() + 
  geom_jitter(width = 0.15) #+
#  scale_color_manual(values = c("green","yellow","red","purple","black"))  #name = "ParcelSeq",

g_cereb_rval_conjGroup_box

g_cereb_rval_conjGroup_box + theme(axis.line = element_line(colour = "black", size = 0.8),
                     panel.grid.major = element_blank(),
                     panel.grid.minor =element_blank(),
                     panel.border = element_blank(),
                     panel.background = element_blank(),
                     axis.title.x = element_blank(), #size = 15,face = "bold"
                     axis.text.x = element_text(size = 12, face = "bold"), #, angle = 0, hjust = 0
                     axis.title.y = element_text(size = 15,face = "bold"),
                     axis.text.y = element_text(size = 12,face = "bold"),
                     legend.position = "none") +
  labs(y = expression('Correlation Values ('~italic(R)~')')) +  #, x = "Parcel"
  scale_x_discrete(labels = c("No conjunction", "Conjunction"))+
  scale_y_continuous(limits = c(-1, 0.7),
                     breaks = seq(-1, 0.7,0.5),
                     expand = c(0,0)) +
  scale_fill_manual(values = c("red", "#0000FF")) +
  geom_text(x =1, y = 0.65, label = "*", size = 6, family = "Times")  


