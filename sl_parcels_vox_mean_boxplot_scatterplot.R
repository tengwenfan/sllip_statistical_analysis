# this script is for voxel mean of each SSL/TSL parcel in both condition (Speech/tone)
# data are extracted from voxel distance = 3 (vox3) parcels (update in Feb 28th)

# rm(list=ls())
library(dplyr);
library(tidyverse);
library(stats);
library(psych);
library(ggpubr);
library(tableone);
library("ggpubr");
library(ggplot2);
library(rstatix);
library(flextable);



subject_ID <- read.csv("/Volumes/lendlab_raid/projects/sllip/make_parcels-tengwen/subjectsID30.csv")


## -----------------------------SSL str>rand parcels----------------------------------------------------------------------

ssl_str_rand_5parcels_speech <- read.csv("/Volumes/lendlab_raid/projects/sllip/make_parcels-tengwen/data/corr_analysis_results_ssl_str_rd_vox3_nosmoothing/task1_mean_in_parcel.csv")
ssl_str_rand_5parcels_tone <- read.csv("/Volumes/lendlab_raid/projects/sllip/make_parcels-tengwen/data/corr_analysis_results_ssl_str_rd_vox3_nosmoothing/task2_mean_in_parcel.csv")

# ssl_parcel1_speech <- ssl_str_rand_5parcels_speech$parcel_1
# ssl_parcel1_tone <- ssl_str_rand_5parcels_tone$parcel_1

# cbind subject_ID
ssl_str_rand_5parcels_speech <- cbind(subject_ID, ssl_str_rand_5parcels_speech)
ssl_str_rand_5parcels_tone <- cbind(subject_ID, ssl_str_rand_5parcels_tone)

ssl_str_rand_5parcels_speech_long <- pivot_longer(
  ssl_str_rand_5parcels_speech,
  cols = c(parcel_1, parcel_3, parcel_5, parcel_8, parcel_13),
  names_to = "Parcel",
  values_to = "Voxel_Mean_Acitivity",  
  names_transform = list(Parcel = as.factor)) %>%
  mutate(subject_ID = as.factor(subject_ID)
  )
# final organized speech data 
ssl_str_rand_5parcels_speech_long <- ssl_str_rand_5parcels_speech_long %>%
  add_column(Condition = "Speech")


ssl_str_rand_5parcels_tone_long <- pivot_longer(
  ssl_str_rand_5parcels_tone,
  cols = c(parcel_1, parcel_3, parcel_5, parcel_8, parcel_13),
  names_to = "Parcel",
  values_to = "Voxel_Mean_Acitivity",
  names_transform = list(Parcel = as.factor)) %>%
  mutate(subject_ID = as.factor(subject_ID)
)

ssl_str_rand_5parcels_tone_long <- ssl_str_rand_5parcels_tone_long %>%
  add_column(Condition = "Tone")

ssl_str_rand_5parcels_voxel_means <-rbind(ssl_str_rand_5parcels_speech_long, ssl_str_rand_5parcels_tone_long)   


ssl_str_rand_5parcels_voxel_means$ParcelSeq <- with(ssl_str_rand_5parcels_voxel_means, ifelse(Parcel == 'parcel_1', 'parcel01',
                                                                                              ifelse(Parcel == 'parcel_3', 'parcel03',
                                                                                                     ifelse(Parcel == 'parcel_5', 'parcel05',
                                                                                                            ifelse(Parcel == 'parcel_8', 'parcel08','parcel13')))))


ssl_str_rand_5parcels_voxel_means$ParcelSeq <- factor(ssl_str_rand_5parcels_voxel_means$ParcelSeq)

ssl_str_rand_5parcels_voxel_means$plotSeq <- with(ssl_str_rand_5parcels_voxel_means, ifelse(Parcel == 'parcel_1', 'plot3',
                                                                                              ifelse(Parcel == 'parcel_3', 'plot1',
                                                                                                     ifelse(Parcel == 'parcel_5', 'plot4',
                                                                                                            ifelse(Parcel == 'parcel_8', 'plot5','plot2')))))



# 
g_ssl <- ggplot(ssl_str_rand_5parcels_voxel_means, aes(plotSeq, Voxel_Mean_Acitivity, linetype=Condition, color = plotSeq)) +
   geom_boxplot(lwd = 1) + 
  #scale_linetype_manual(name = "Condition", values = c("solid", "dashed"))+
  scale_color_manual(values = c("#F46D43","#5E4FA2","#9E0142","#FEE08B","#E6F598"), guide = "none")  #name = "ParcelSeq",
  
g_ssl

g_ssl + theme(axis.line = element_line(colour = "black", size = 0.8),
              panel.grid.major = element_blank(),
              panel.grid.minor =element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.title.x = element_blank(),
              #axis.title.x = element_text(size = 15,face = "bold"),
              axis.text.x = element_text(size = 12, face = "bold",angle = 15, hjust = 0.9),
              axis.title.y = element_text(size = 15,face = "bold"),
              axis.text.y = element_text(size = 12,face = "bold")
) +
  scale_y_continuous(limits = c(-800, 830),
                     breaks = seq(-800,830,400),
                     expand = c(0,0)) +
  labs(y = "Mean Voxel Magnitude", x = "Parcel") +
  scale_x_discrete(labels = c("Left STG/\nAngular Gyrus", "Right STG", "Right Cerebellum", "Right Frontal Pole", "Right IFG"))+
  scale_fill_discrete(
    name = "Condition",
    labels = c("Speech", "Group")
  ) +
 theme(legend.position = c(.9,.1),
       legend.text=element_text(size=12),
       legend.title = element_blank(),
       legend.background = element_rect(fill = "white"),
       legend.key.size = unit(0.8, 'cm'),
       legend.key=element_blank()
       ) +
  # plot 1/parcel 3; plot2 /parcel13; plot3/parcel1; plot4/parcel5; plot5/parcel8
  geom_text(x = 1, y = 790, label = "*", size = 6, family = "Times", color = "black") +
  geom_text(x = 2, y = 790, label = "*", size = 6, family = "Times", color = "black") +   
  geom_text(x = 5, y = 790, label = "†", size = 5, family = "Times", color = "black") 
  
  
  
  #scale_linetype_manual(name = "Condition", values = c("solid", "dashed")) #+
 # geom_line(aes(linetype = Condition))


## another way to plot boxplot to add sig automatically: ggboxplot------------------------------
ssl_str_rand_5parcels_voxel_means$Condition <- factor(ssl_str_rand_5parcels_voxel_means$Condition)
ssl_str_rand_5parcels_voxel_means_noID <- ssl_str_rand_5parcels_voxel_means[-1]
stat_test <- ssl_str_rand_5parcels_voxel_means_noID %>%
  group_by(Parcel) %>%
  t_test(Voxel_Mean_Acitivity ~ Condition) %>%
  mutate(y.position = 750)


g_ssl2 <- ggboxplot(
  ssl_str_rand_5parcels_voxel_means, x = "Condition", y = "Voxel_Mean_Acitivity",
  color = "Parcel", facet.by = "Parcel"
)
g_ssl2 + 
  stat_pvalue_manual(stat_test, label = "p")


#-----------------------------------------------------------------------------------------------

# t test
t.test(ssl_str_rand_5parcels_speech$parcel_1,
       ssl_str_rand_5parcels_tone$parcel_1,
       alternative="two.sided",
       paired = TRUE,
       var.equal = FALSE
)


t.test(ssl_str_rand_5parcels_speech$parcel_3,
       ssl_str_rand_5parcels_tone$parcel_3,
       alternative="two.sided",
       paired = TRUE,
       var.equal = FALSE
)

t.test(ssl_str_rand_5parcels_speech$parcel_5,
       ssl_str_rand_5parcels_tone$parcel_5,
       alternative="two.sided",
       paired = TRUE,
       var.equal = FALSE
)

t.test(ssl_str_rand_5parcels_speech$parcel_8,
       ssl_str_rand_5parcels_tone$parcel_8,
       alternative="two.sided",
       paired = TRUE,
       var.equal = FALSE
)


t.test(ssl_str_rand_5parcels_speech$parcel_13,
       ssl_str_rand_5parcels_tone$parcel_13,
       alternative="two.sided",
       paired = TRUE,
       var.equal = FALSE
)


## --------scatter plot

ssl_str_rand_5parcels_voxel_means_wider <- pivot_wider(
  ssl_str_rand_5parcels_voxel_means,
  names_from = Condition,
  values_from = Voxel_Mean_Acitivity
)
  
ssl_scatterplot <- ggscatter(ssl_str_rand_5parcels_voxel_means_wider, x = "Speech", y = "Tone",
                                                    facet.by = "Parcel",
                                                    add = "reg.line", conf.int = TRUE,
                                                    cor.coef = TRUE, cor.method = "pearson",
                                                    xlab = "Speech", ylab = "Tone") +
  theme(axis.line = element_line(colour = "black", size = 0.8),
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
  scale_y_continuous(limits = c(-800, 800), # this range will remove some data points, I think we can check if those point are influencers or outliers
                     breaks = seq(-800, 800,100),
                     expand = c(0,0))


## pick one scatter plot

ssl_str_rand_5parcels_voxel_means_wider_parcel_3 <- ssl_str_rand_5parcels_voxel_means_wider[ssl_str_rand_5parcels_voxel_means_wider$Parcel == "parcel_3", ]

ssl_scatterplot_parcel_3 <- ggscatter(ssl_str_rand_5parcels_voxel_means_wider_parcel_3, x = "Speech", y = "Tone",
                             #facet.by = "Parcel",
                             color = "#F46D43",
                             add = "reg.line", conf.int = TRUE,
                             #cor.coef = TRUE, cor.method = "pearson",
                             xlab = "Speech", ylab = "Tone") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), show.legend = FALSE, size = 5.5) +
  #stat_cor(aes(color = "#F46D43", label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), show.legend = FALSE, size = 5.5) +
  theme(axis.line = element_line(colour = "black", size = 0.8),
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
  scale_y_continuous(limits = c(-350, 200), # this range will remove some data points, I think we can check if those point are influencers or outliers
                     breaks = seq(-350, 200,100),
                     expand = c(0,0)) +
  geom_text(x = -70, y = -300, label = "Left STG/Angular Gyrus", size =6, family = "Times") +
  labs(y = "Tone Mean Voxel Magnitude", x = "Speech Mean Voxel Magnitude") 




#---------------------------TSL str>rand parcels---------------------------------------------------------------------

tsl_str_rand_2parcels_speech <- read.csv("/Volumes/lendlab_raid/projects/sllip/make_parcels-tengwen/data/corr_analysis_results_tsl_str_rd_vox3_nosmoothing/task1_mean_in_parcel.csv")
tsl_str_rand_2parcels_tone <- read.csv("/Volumes/lendlab_raid/projects/sllip/make_parcels-tengwen/data/corr_analysis_results_tsl_str_rd_vox3_nosmoothing/task2_mean_in_parcel.csv")

tsl_str_rand_2parcels_speech <- cbind(subject_ID, tsl_str_rand_2parcels_speech)
tsl_str_rand_2parcels_tone <- cbind(subject_ID, tsl_str_rand_2parcels_tone)


tsl_str_rand_2parcels_speech_long <- pivot_longer(
  tsl_str_rand_2parcels_speech,
  cols = c(parcel_5, parcel_7),
  names_to = "Parcel",
  values_to = "Voxel_Mean_Acitivity",  
  names_transform = list(Parcel = as.factor)) %>%
  mutate(subject_ID = as.factor(subject_ID)
  )

# final organized speech data 
tsl_str_rand_2parcels_speech_long <- tsl_str_rand_2parcels_speech_long %>%
  add_column(Condition = "Speech")



tsl_str_rand_2parcels_tone_long <- pivot_longer(
  tsl_str_rand_2parcels_tone,
  cols = c(parcel_5, parcel_7),
  names_to = "Parcel",
  values_to = "Voxel_Mean_Acitivity",  
  names_transform = list(Parcel = as.factor)) %>%
  mutate(subject_ID = as.factor(subject_ID)
  )

# final organized speech data 
tsl_str_rand_2parcels_tone_long <- tsl_str_rand_2parcels_tone_long %>%
  add_column(Condition = "Tone")


tsl_str_rand_2parcels_voxel_means <-rbind(tsl_str_rand_2parcels_speech_long, tsl_str_rand_2parcels_tone_long)

tsl_str_rand_2parcels_voxel_means$plotSeq <- with(tsl_str_rand_2parcels_voxel_means, ifelse(Parcel == 'parcel_5', 'plot1','plot2'))



# 
g_tsl <- ggplot(tsl_str_rand_2parcels_voxel_means, aes(plotSeq, Voxel_Mean_Acitivity, linetype=Condition, color = plotSeq)) +
  geom_boxplot(lwd = 1) +
  scale_color_manual(values = c("#008080","#FF8080"), guide = "none")
g_tsl


g_tsl + theme(axis.line = element_line(colour = "black", size = 0.8),
      panel.grid.major = element_blank(),
      panel.grid.minor =element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.title.x = element_blank(),
      #axis.title.x = element_text(size = 15,face = "bold"),
      axis.text.x = element_text(size = 12, face = "bold",angle = 15, hjust = 0.9),
      axis.title.y = element_text(size = 15,face = "bold"),
      axis.text.y = element_text(size = 12,face = "bold")
) +
  scale_y_continuous(limits = c(-200, 230),
                     breaks = seq(-200,230,100),
                     expand = c(0,0)) +
  labs(y = "Mean Voxel Magnitude", x = "Parcel") +
  scale_x_discrete(labels = c("Cingulate Gyrus", "Left Frontal Pole"))+
  scale_fill_discrete(
    name = "Condition",
    labels = c("Speech", "Group")
  ) +
  theme(legend.position = c(.85,.1),
        legend.text=element_text(size=12),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.key.size = unit(0.8, 'cm'),
        legend.key=element_blank()) +
  geom_text(x = 1, y = 205, label = "†", size = 5, family = "Times", color = "black") +
  geom_text(x = 2, y = 205, label = "*", size = 6, family = "Times", color = "black") 

# --------------------------copy----------------------




# ---------------copy ------------------------------------



# t test
t.test(tsl_str_rand_2parcels_speech$parcel_5,
       tsl_str_rand_2parcels_tone$parcel_5,
       alternative="two.sided",
       paired = TRUE,
       var.equal = FALSE
)


t.test(tsl_str_rand_2parcels_speech$parcel_7,
       tsl_str_rand_2parcels_tone$parcel_7,
       alternative="two.sided",
       paired = TRUE,
       var.equal = FALSE
)

# scatter plot
tsl_str_rand_2parcels_voxel_means_wider <- pivot_wider(
  tsl_str_rand_2parcels_voxel_means,
  names_from = Condition,
  values_from = Voxel_Mean_Acitivity
)

tsl_scatterplot <- ggscatter(tsl_str_rand_2parcels_voxel_means_wider, x = "Speech", y = "Tone",
                             facet.by = "Parcel",
                             add = "reg.line", conf.int = TRUE,
                             cor.coef = TRUE, cor.method = "pearson",
                             xlab = "Speech", ylab = "Tone") +
  theme(axis.line = element_line(colour = "black", size = 0.8),
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
  scale_y_continuous(limits = c(-300, 300), # this range will remove some data points, I think we can check if those point are influencers or outliers
                     breaks = seq(-300, 300,100),
                     expand = c(0,0))

# pick one scatter plot

tsl_str_rand_2parcels_voxel_means_wider_parcel_5 <- tsl_str_rand_2parcels_voxel_means_wider[tsl_str_rand_2parcels_voxel_means_wider$Parcel == "parcel_5", ]

tsl_scatterplot_parcel_5 <- ggscatter(tsl_str_rand_2parcels_voxel_means_wider_parcel_5, x = "Speech", y = "Tone",
                                      #facet.by = "Parcel",
                                      color = "#008080",
                                      add = "reg.line", conf.int = TRUE,
                                      #cor.coef = TRUE, cor.method = "pearson",
                                      xlab = "Speech", ylab = "Tone") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), show.legend = FALSE, size = 5.5) +
  theme(axis.line = element_line(colour = "black", size = 0.8),
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
  scale_y_continuous(limits = c(-200, 200), # this range will remove some data points, I think we can check if those point are influencers or outliers
                     breaks = seq(-200, 200,100),
                     expand = c(0,0)) +
  geom_text(x = -145, y = -150, label = "Cingulate Gyrus", size =6, family = "Times") +  #sans/mono 
  labs(y = "Tone Mean Voxel Magnitude", x = "Speech Mean Voxel Magnitude")


# parcel_7
tsl_str_rand_2parcels_voxel_means_wider_parcel_7 <- tsl_str_rand_2parcels_voxel_means_wider[tsl_str_rand_2parcels_voxel_means_wider$Parcel == "parcel_7", ]

tsl_scatterplot_parcel_7 <- ggscatter(tsl_str_rand_2parcels_voxel_means_wider_parcel_7, x = "Speech", y = "Tone",
                                      #facet.by = "Parcel",
                                      color = "#FF8080",
                                      add = "reg.line", conf.int = TRUE,
                                      #cor.coef = TRUE, cor.method = "pearson",
                                      xlab = "Speech", ylab = "Tone") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), show.legend = FALSE, size = 5.5) +
  theme(axis.line = element_line(colour = "black", size = 0.8),
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
  scale_y_continuous(limits = c(-200, 200), # this range will remove some data points, I think we can check if those point are influencers or outliers
                     breaks = seq(-200, 200,100),
                     expand = c(0,0)) +
  geom_text(x = -80, y = -150, label = "Left Frontal Pole", size =6, family = "Times") +  #sans/mono 
  labs(y = "Tone Mean Voxel Magnitude", x = "Speech Mean Voxel Magnitude")



## ------------------within cerebellum parcel 2-----------------------------------------------------------------------------

cerebellum_parcel2_speech <- read.csv("/Volumes/lendlab_raid/projects/sllip/make_parcels-tengwen/data/corr_analysis_results_cerebellumparcel2_vox3_nosmoothing_0301/task1_mean_in_parcel.csv")
cerebellum_parcel2_tone <- read.csv("/Volumes/lendlab_raid/projects/sllip/make_parcels-tengwen/data/corr_analysis_results_cerebellumparcel2_vox3_nosmoothing_0301/task2_mean_in_parcel.csv")


cerebellum_parcel2_speech <- cbind(subject_ID, cerebellum_parcel2_speech)
cerebellum_parcel2_tone <- cbind(subject_ID, cerebellum_parcel2_tone)


cerebellum_parcel2_speech_long <- pivot_longer(
  cerebellum_parcel2_speech,
  cols = c(parcel_2),
  names_to = "Parcel",
  values_to = "Voxel_Mean_Acitivity",  
  names_transform = list(Parcel = as.factor)) %>%
  mutate(subject_ID = as.factor(subject_ID)
  )

# final organized speech data 
cerebellum_parcel2_speech_long <- cerebellum_parcel2_speech_long %>%
  add_column(Condition = "Speech")


# tone
cerebellum_parcel2_tone_long <- pivot_longer(
  cerebellum_parcel2_tone,
  cols = c(parcel_2),
  names_to = "Parcel",
  values_to = "Voxel_Mean_Acitivity",  
  names_transform = list(Parcel = as.factor)) %>%
  mutate(subject_ID = as.factor(subject_ID)
  )

# final organized speech data 
cerebellum_parcel2_tone_long <- cerebellum_parcel2_tone_long %>%
  add_column(Condition = "Tone")

# merge speech and tone
cerebellum_parcel2_voxel_means <-rbind(cerebellum_parcel2_speech_long, cerebellum_parcel2_tone_long)   

# 
g_cereb <- ggplot(cerebellum_parcel2_voxel_means, aes(Parcel, Voxel_Mean_Acitivity, linetype=Condition, color = Parcel)) +
  geom_boxplot(lwd = 1) +
  scale_color_manual(values = c("#E0C2F2"), guide = "none") 

g_cereb


g_cereb + theme(axis.line = element_line(colour = "black", size = 0.8),
              panel.grid.major = element_blank(),
              panel.grid.minor =element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.title.x = element_blank(),
              #axis.title.x = element_text(size = 15,face = "bold"),
              axis.text.x = element_text(size = 12, face = "bold"), #,angle = 15, hjust = 0.9
              axis.title.y = element_text(size = 15,face = "bold"),
              axis.text.y = element_text(size = 12,face = "bold")
) +
  scale_y_continuous(limits = c(-500, 750),
                     breaks = seq(-500,750,100),
                     expand = c(0,0)) +
  labs(y = "Mean Voxel Magnitude", x = "Parcel") +
  scale_x_discrete(labels = c("Cerebellum"))+
  scale_fill_discrete(
    name = "Condition",
    labels = c("Speech", "Group")
  ) +
  theme(legend.position = c(.85,.1),
        legend.text=element_text(size=12),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.key.size = unit(0.8, 'cm'),
        legend.key=element_blank()) 

# t test
t.test(cerebellum_parcel2_speech$parcel_2,
       cerebellum_parcel2_tone$parcel_2,
       alternative="two.sided",
       paired = TRUE,
       var.equal = FALSE
)



# scatter plot

cerebellum_parcel2_voxel_means_wider <- pivot_wider(
  cerebellum_parcel2_voxel_means,
  names_from = Condition,
  values_from = Voxel_Mean_Acitivity
)


cerebellum_scatterplot <- ggscatter(cerebellum_parcel2_voxel_means_wider, x = "Speech", y = "Tone",
                             #facet.by = "Parcel",
                             color = "#E0C2F2",
                             add = "reg.line", conf.int = TRUE,
                             #cor.coef = TRUE, cor.method = "pearson",
                             xlab = "Speech", ylab = "Tone") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), show.legend = FALSE, size = 5.5) +
  theme(axis.line = element_line(colour = "black", size = 0.8),
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
  scale_y_continuous(limits = c(-500, 750), # this range will remove some data points, I think we can check if those point are influencers or outliers
                     breaks = seq(-500, 750,100),
                     expand = c(0,0)) +
  geom_text(x = -210, y = -250, label = "Cerebellum", size =6, family = "Times")  +   #sans/mono
  labs(y = "Tone Mean Voxel Magnitude", x = "Speech Mean Voxel Magnitude")



# --------- cerebellum: group * condition ---------
subject_ID_conjGroup <- read.csv('/Volumes/lendlab_raid/projects/sllip/make_parcels-tengwen/cerebellum_group_subject_ID.csv')

# !!pay attention that after merge, the sequence of row names becomes not consistent with the sequence of subject_ID
cerebellum_parcel2_speech_long_conjGroup=merge(cerebellum_parcel2_speech_long,subject_ID_conjGroup,by='subject_ID', all=TRUE, sort=TRUE,suffixes = c(".voxelmean",".group")) 
cerebellum_parcel2_tone_long_conjGroup=merge(cerebellum_parcel2_tone_long,subject_ID_conjGroup,by='subject_ID', all=TRUE, sort=TRUE,suffixes = c(".voxelmean",".group")) 

cerebellum_parcel2_voxel_means_conjGroup <-rbind(cerebellum_parcel2_speech_long_conjGroup, cerebellum_parcel2_tone_long_conjGroup)   

# 
g_cereb_conjGroup <- ggplot(cerebellum_parcel2_voxel_means_conjGroup, aes(group, Voxel_Mean_Acitivity, linetype=Condition, color = group)) +
  geom_boxplot(lwd = 1) +
  scale_color_manual(values = c("red", "#0000FF"), guide = "none") 

g_cereb_conjGroup


g_cereb_conjGroup + theme(axis.line = element_line(colour = "black", size = 0.8),
                panel.grid.major = element_blank(),
                panel.grid.minor =element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.title.x = element_blank(),
                #axis.title.x = element_text(size = 15,face = "bold"),
                axis.text.x = element_text(size = 12, face = "bold"), #,angle = 15, hjust = 0.9
                axis.title.y = element_text(size = 15,face = "bold"),
                axis.text.y = element_text(size = 12,face = "bold")
) +
  scale_y_continuous(limits = c(-500, 750),
                     breaks = seq(-500,750,100),
                     expand = c(0,0)) +
  labs(y = "Mean Voxel Magnitude") + # , x = "Parcel"
  scale_x_discrete(labels = c("No conjunction", "Conjunction"))+
  scale_fill_discrete(
    name = "Condition",
    labels = c("Speech", "Group")
  ) +
  theme(legend.position = c(.85,.1),
        legend.text=element_text(size=12),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.key.size = unit(0.8, 'cm'),
        legend.key=element_blank()) +
  geom_text(x = 2, y = 725, label = "*", size = 5, family = "Times", color = "black")


# t test
t.test(cerebellum_parcel2_voxel_means_conjGroup$Voxel_Mean_Acitivity[cerebellum_parcel2_voxel_means_conjGroup$group == "insig" & cerebellum_parcel2_voxel_means_conjGroup$Condition == "Speech"],
       cerebellum_parcel2_voxel_means_conjGroup$Voxel_Mean_Acitivity[cerebellum_parcel2_voxel_means_conjGroup$group == "insig" & cerebellum_parcel2_voxel_means_conjGroup$Condition == "Tone"],
       alternative="two.sided",
       paired = TRUE,
       var.equal = FALSE
)


t.test(cerebellum_parcel2_voxel_means_conjGroup$Voxel_Mean_Acitivity[cerebellum_parcel2_voxel_means_conjGroup$group == "sig" & cerebellum_parcel2_voxel_means_conjGroup$Condition == "Speech"],
       cerebellum_parcel2_voxel_means_conjGroup$Voxel_Mean_Acitivity[cerebellum_parcel2_voxel_means_conjGroup$group == "sig" & cerebellum_parcel2_voxel_means_conjGroup$Condition == "Tone"],
       alternative="two.sided",
       paired = TRUE,
       var.equal = FALSE
)

# one sample t test
t.test(cerebellum_parcel2_voxel_means_conjGroup$Voxel_Mean_Acitivity[cerebellum_parcel2_voxel_means_conjGroup$group == "insig" & cerebellum_parcel2_voxel_means_conjGroup$Condition == "Speech"],mu = 0,alternative = "two.sided")
t.test(cerebellum_parcel2_voxel_means_conjGroup$Voxel_Mean_Acitivity[cerebellum_parcel2_voxel_means_conjGroup$group == "insig" & cerebellum_parcel2_voxel_means_conjGroup$Condition == "Tone"],mu = 0,alternative = "two.sided")
t.test(cerebellum_parcel2_voxel_means_conjGroup$Voxel_Mean_Acitivity[cerebellum_parcel2_voxel_means_conjGroup$group == "sig" & cerebellum_parcel2_voxel_means_conjGroup$Condition == "Speech"],mu = 0,alternative = "two.sided")
t.test(cerebellum_parcel2_voxel_means_conjGroup$Voxel_Mean_Acitivity[cerebellum_parcel2_voxel_means_conjGroup$group == "sig" & cerebellum_parcel2_voxel_means_conjGroup$Condition == "Tone"],mu = 0,alternative = "two.sided")



# scatter plot



cerebellum_parcel2_voxel_means_conjGroup_wider <- pivot_wider(
  cerebellum_parcel2_voxel_means_conjGroup,
  names_from = Condition,
  values_from = Voxel_Mean_Acitivity
)


cerebellum_parcel2_voxel_means_cor_group <-ggscatter(cerebellum_parcel2_voxel_means_conjGroup_wider, x = "Speech", y = "Tone",
                                            color = "group", #size = 4, #shape = "sig",
                                            add = "reg.line", conf.int = TRUE,
                                            # cor.coef = TRUE, cor.method = "pearson",
                                            xlab = "Speech", ylab = "Tone"
                                            ) + #palette = c("#0000FF", "#EE4E34")
  scale_color_manual(labels = c("No Conjunction", "Conjunction"), values = c("red", "#0000FF"))+
  scale_fill_manual(labels = c("No Conjunction", "Conjunction"), values = c("red", "#0000FF")) +
  stat_cor(aes(color = group, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           show.legend = FALSE, 
           size = 4.5,
           label.x.npc = "middle",
           label.y.npc = "top") +
  # scale_color_discrete(
  #   name = "Group",
  #   labels = c("No Conjunction", "Conjunction")
  #   ) +
  # scale_fill_discrete(
  #   name = "Group",
  #   labels = c("No Conjunction", "Conjunction")
  #   ) +
  scale_x_continuous(name="Speech Mean Voxel Magnitude") +
  scale_y_continuous(name="Tone Mean Voxel Magnitude") +
  theme(axis.title.x = element_text(size = 15,face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 15,face = "bold"),
        axis.text.y = element_text(size = 12,face = "bold"),
        legend.text=element_text(size=11),
        legend.title = element_blank(),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.margin=margin(15,0,0,0)
        )
  

cerebellum_parcel2_voxel_means_cor_group

# theme(legend.position = c(.85,.1),
#       legend.text=element_text(size=12),
#       legend.title = element_blank(),
#       legend.background = element_rect(fill = "white"),
#       legend.key.size = unit(0.8, 'cm'),
#       legend.key=element_blank())




