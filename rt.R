
# rm(list=ls())
library(dplyr)
library(tidyverse)
library(stats)
library(psych)
library(ggpubr);

# analysis 1
rt <- read.csv("mean_rt.csv")
str(rt)
rt_mri <- subset(rt, rt$mri_id == 1)


mean(rt_mri$random_syllable, na.rm = TRUE)
mean(rt_mri$random_tone, na.rm = TRUE)
mean(rt_mri$structured_syllable, na.rm = TRUE)
mean(rt_mri$structured_tone, na.rm = TRUE)

sd(rt_mri$random_syllable, na.rm = TRUE)
sd(rt_mri$random_tone, na.rm = TRUE)
sd(rt_mri$structured_syllable, na.rm = TRUE)
sd(rt_mri$structured_tone, na.rm = TRUE)

rt_mri$speech_str_rand <- rt_mri$structured_syllable - rt_mri$random_syllable
rt_mri$tone_str_rand <- rt_mri$structured_tone - rt_mri$random_tone

t.test(rt_mri$speech_str_rand, mu = 0,alternative = "two.sided")
t.test(rt_mri$tone_str_rand, mu = 0,alternative = "two.sided")


t.test(rt_mri$structured_syllable,
       rt_mri$random_syllable,
       alternative="two.sided",
       paired = TRUE,
       var.equal = FALSE
)

t.test(rt_mri$structured_tone,
       rt_mri$random_tone,
       alternative="two.sided",
       paired = TRUE,
       var.equal = FALSE
)


## analysis 2------------------------------------------------------------------------
# install.packages("jtools")
library("jtools")
library("psych")
pvt <- read.csv("SLLIP_clean_data_sl_froi.csv")
merge_rt_pvt=merge(pvt,rt,by='subject_ID', all=TRUE, sort=TRUE,suffixes = c("_sllip","_rt"))


merge_rt_pvt_mri <- subset(merge_rt_pvt, merge_rt_pvt$mri_id == 1)

# partial correlation among pvt, rt and froi in sl parcels
par.r.input<-partial.r(merge_rt_pvt_mri, x=c('nih_pvt_uncorrected','random_syllable','random_tone','structured_syllable','structured_tone', 'parcel_1_ssl_str_rd_froi','parcel_3_ssl_str_rd_froi', 'parcel_5_ssl_str_rd_froi','parcel_8_ssl_str_rd_froi', 'parcel_13_ssl_str_rd_froi','parcel_5_tsl_str_rd_froi','parcel_7_tsl_str_rd_froi'), y=c("chrono_age"))
lowerMat(par.r.input)


# par.r.input<-partial.r(merge_rt_pvt_mri, x=c('nih_pvt_uncorrected','random_syllable','random_tone','structured_syllable','structured_tone','parcel_7_ssl_rd_str_froi', 'parcel_39_ssl_rd_str_froi', 'parcel_59_ssl_rd_str_froi', 'parcel_1_ssl_str_rd_froi','parcel_3_ssl_str_rd_froi', 'parcel_5_ssl_str_rd_froi','parcel_8_ssl_str_rd_froi', 'parcel_13_ssl_str_rd_froi','parcel_5_tsl_str_rd_froi','parcel_7_tsl_str_rd_froi'), y=c("chrono_age"))
# par.r.input<-partial.r(merge_rt_pvt_mri, x=c('nih_pvt_uncorrected','parcel_1_ssl_str_rd_froi'), y=c("chrono_age"))

# summary(lm(merge_rt_pvt$nih_pvt_uncorrected ~ merge_rt_pvt$chrono_age + merge_rt_pvt$nih_pvt_uncorrected))
# cp <- corr.p(par.r.input,n=98)
# print(cp, short = TRUE)


## MRIsub_rt_NA -----------------------------------------------------
rt_mri_NA <- rt$subject_ID[rt$mri_id ==1 & (is.na(rt$random_syllable) | is.na(rt$random_tone) | is.na(rt$structured_syllable) | is.na(rt$structured_tone)) ]
data.frame(rt_mri_NA)

rt_mri_NA_randsyl <- rt$subject_ID[rt$mri_id ==1 & is.na(rt$random_syllable)]
data.frame(rt_mri_NA_randsyl)

rt_mri_NA_randtone <- rt$subject_ID[rt$mri_id ==1 & is.na(rt$random_tone)]
data.frame(rt_mri_NA_randtone)

rt_mri_NA_strsyl <- rt$subject_ID[rt$mri_id ==1 & is.na(rt$structured_syllable)]
data.frame(rt_mri_NA_strsyl)

rt_mri_NA_strtone <- rt$subject_ID[rt$mri_id ==1 & is.na(rt$structured_tone)]
data.frame(rt_mri_NA_strtone)



## partial correlation among pvt, rt, froi in sl parcels, froi in cerebellum 
cereb <- read.csv("leftCerebtop10.csv")
merge_rt_pvt_cereb=merge(merge_rt_pvt,cereb,by='subject_ID', all=TRUE, sort=TRUE,suffixes = c("_sllip","_cereb"))

merge_rt_pvt_cereb$speech_str_rand <- merge_rt_pvt_cereb$structured_syllable - merge_rt_pvt_cereb$random_syllable
merge_rt_pvt_cereb$tone_str_rand <- merge_rt_pvt_cereb$structured_tone - merge_rt_pvt_cereb$random_tone

 write.csv(merge_rt_pvt_cereb, "merge_rt_pvt_cereb.csv", row.names = TRUE)

merge_rt_pvt_cereb_mri <- subset(merge_rt_pvt_cereb, merge_rt_pvt_cereb$mri_id == 1) ## !!!!!!!!


#par.r.input<-partial.r(merge_rt_pvt_cereb_mri, x=c('nih_pvt_uncorrected','random_syllable','random_tone','structured_syllable','structured_tone', 'parcel_1_ssl_str_rd_froi','parcel_3_ssl_str_rd_froi', 'parcel_5_ssl_str_rd_froi','parcel_8_ssl_str_rd_froi', 'parcel_13_ssl_str_rd_froi','parcel_5_tsl_str_rd_froi','parcel_7_tsl_str_rd_froi','cereb_parcel2_tsl_rand_str', 'cereb_parcel2_tsl_str_rand', 'cereb_parcel2_ssl_str_rand','cereb_parcel2_ssl_rand_str'), y=c("chrono_age"))
#lowerMat(par.r.input)

#write.csv(par.r.input, "par_r_input.csv")



par_r_RT_diff<-partial.r(merge_rt_pvt_cereb_mri, x=c('nih_pvt_uncorrected','speech_str_rand','tone_str_rand', 'parcel_1_ssl_str_rd_froi','parcel_3_ssl_str_rd_froi', 'parcel_5_ssl_str_rd_froi','parcel_8_ssl_str_rd_froi', 'parcel_13_ssl_str_rd_froi','parcel_5_tsl_str_rd_froi','parcel_7_tsl_str_rd_froi','cereb_parcel2_tsl_rand_str', 'cereb_parcel2_tsl_str_rand', 'cereb_parcel2_ssl_str_rand','cereb_parcel2_ssl_rand_str'), y=c("chrono_age"))
lowerMat(par_r_RT_diff)

write.csv(par_r_RT_diff, "par_r_RT_diff.csv")

## 01242023----------------------------------------------

# write.csv(merge_rt_pvt_cereb_mri, "merge_rt_pvt_cereb_mri.csv")
# merge_rt_pvt_cereb_mri <- read.csv("merge_rt_pvt_cereb_mri.csv")

## age of participants included in RT analysis in each condition
mean(merge_rt_pvt_cereb_mri$chrono_age[!is.na(merge_rt_pvt_cereb_mri$structured_syllable)], na.rm = TRUE)
mean(merge_rt_pvt_cereb_mri$chrono_age[!is.na(merge_rt_pvt_cereb_mri$structured_tone)], na.rm = TRUE)
mean(merge_rt_pvt_cereb_mri$chrono_age[!is.na(merge_rt_pvt_cereb_mri$random_syllable)], na.rm = TRUE)
mean(merge_rt_pvt_cereb_mri$chrono_age[!is.na(merge_rt_pvt_cereb_mri$random_tone)], na.rm = TRUE)


sd(merge_rt_pvt_cereb_mri$chrono_age[!is.na(merge_rt_pvt_cereb_mri$structured_syllable)], na.rm = TRUE)
sd(merge_rt_pvt_cereb_mri$chrono_age[!is.na(merge_rt_pvt_cereb_mri$structured_tone)], na.rm = TRUE)
sd(merge_rt_pvt_cereb_mri$chrono_age[!is.na(merge_rt_pvt_cereb_mri$random_syllable)], na.rm = TRUE)
sd(merge_rt_pvt_cereb_mri$chrono_age[!is.na(merge_rt_pvt_cereb_mri$random_tone)], na.rm = TRUE)


mean(merge_rt_pvt_cereb_mri$chrono_age[!is.na(merge_rt_pvt_cereb_mri$structured_syllable)|!is.na(merge_rt_pvt_cereb_mri$random_syllable)], na.rm = TRUE)
sd(merge_rt_pvt_cereb_mri$chrono_age[!is.na(merge_rt_pvt_cereb_mri$structured_syllable)|!is.na(merge_rt_pvt_cereb_mri$random_syllable)], na.rm = TRUE)

mean(merge_rt_pvt_cereb_mri$chrono_age[!is.na(merge_rt_pvt_cereb_mri$structured_tone)|!is.na(merge_rt_pvt_cereb_mri$random_tone)], na.rm = TRUE)
sd(merge_rt_pvt_cereb_mri$chrono_age[!is.na(merge_rt_pvt_cereb_mri$structured_tone)|!is.na(merge_rt_pvt_cereb_mri$random_tone)], na.rm = TRUE)


# participants ID in speech condition
ID_mri_NA_speech <- merge_rt_pvt_cereb_mri$subject_ID[(!is.na(merge_rt_pvt_cereb_mri$structured_syllable))|(!is.na(merge_rt_pvt_cereb_mri$random_syllable))]
data.frame(ID_mri_NA_speech) #  participant have rt in structure or random

ID_mri_NA_speech_excluded <- merge_rt_pvt_cereb_mri$subject_ID[(is.na(merge_rt_pvt_cereb_mri$structured_syllable))&(is.na(merge_rt_pvt_cereb_mri$random_syllable))]
data.frame(ID_mri_NA_speech_excluded) # participants do not have rt in both struture and random

ID_mri_NA_speech_excludedonly1 <- merge_rt_pvt_cereb_mri$subject_ID[(is.na(merge_rt_pvt_cereb_mri$structured_syllable))|(is.na(merge_rt_pvt_cereb_mri$random_syllable))]
data.frame(ID_mri_NA_speech_excludedonly1) #participants do not have rt in structure or random

ID_mri_NA_speech_rtinboth <- merge_rt_pvt_cereb_mri$subject_ID[(!is.na(merge_rt_pvt_cereb_mri$structured_syllable))&(!is.na(merge_rt_pvt_cereb_mri$random_syllable))]
data.frame(ID_mri_NA_speech_rtinboth) #participants have rt in both structure or random

# speech condition gender

gender_speech <- merge_rt_pvt_cereb_mri$gender[(!is.na(merge_rt_pvt_cereb_mri$structured_syllable))|(!is.na(merge_rt_pvt_cereb_mri$random_syllable))]
data.frame(gender_speech) #  participant have rt in structure or random

sum(gender_speech == 1, na.rm = TRUE)

gender_speech_excluded <- merge_rt_pvt_cereb_mri$gender[(is.na(merge_rt_pvt_cereb_mri$structured_syllable))&(is.na(merge_rt_pvt_cereb_mri$random_syllable))]
data.frame(gender_speech_excluded) # participants do not have rt in both struture and random

gender_speech_excludedonly1 <- merge_rt_pvt_cereb_mri$gender[(is.na(merge_rt_pvt_cereb_mri$structured_syllable))|(is.na(merge_rt_pvt_cereb_mri$random_syllable))]
data.frame(gender_speech_excludedonly1) #participants do not have rt in structure or random

gender_speech_rtinboth <- merge_rt_pvt_cereb_mri$gender[(!is.na(merge_rt_pvt_cereb_mri$structured_syllable))&(!is.na(merge_rt_pvt_cereb_mri$random_syllable))]
data.frame(gender_speech_rtinboth) #participants have rt in both structure and random

# participants ID in tone condition
ID_mri_NA_tone <- merge_rt_pvt_cereb_mri$subject_ID[(!is.na(merge_rt_pvt_cereb_mri$structured_tone))|(!is.na(merge_rt_pvt_cereb_mri$random_tone))]
data.frame(ID_mri_NA_tone) #  participant have rt in structure or random

ID_mri_NA_tone_excluded <- merge_rt_pvt_cereb_mri$subject_ID[(is.na(merge_rt_pvt_cereb_mri$structured_tone))&(is.na(merge_rt_pvt_cereb_mri$random_tone))]
data.frame(ID_mri_NA_tone_excluded) # participants do not have rt in both struture and random

ID_mri_NA_tone_excludedonly1 <- merge_rt_pvt_cereb_mri$subject_ID[(is.na(merge_rt_pvt_cereb_mri$structured_tone))|(is.na(merge_rt_pvt_cereb_mri$random_tone))]
data.frame(ID_mri_NA_tone_excludedonly1) #participants do not have rt in structure or random

ID_mri_NA_tone_rtinboth <- merge_rt_pvt_cereb_mri$subject_ID[(!is.na(merge_rt_pvt_cereb_mri$structured_tone))&(!is.na(merge_rt_pvt_cereb_mri$random_tone))]
data.frame(ID_mri_NA_tone_rtinboth) #participants have rt in both structure or random


# gender in tone condition
gender_tone <- merge_rt_pvt_cereb_mri$gender[(!is.na(merge_rt_pvt_cereb_mri$structured_tone))|(!is.na(merge_rt_pvt_cereb_mri$random_tone))]
data.frame(gender_tone) #  participant have rt in structure or random

sum(gender_tone == 1, na.rm = TRUE)

gender_tone_excluded <- merge_rt_pvt_cereb_mri$gender[(is.na(merge_rt_pvt_cereb_mri$structured_tone))&(is.na(merge_rt_pvt_cereb_mri$random_tone))]
data.frame(gender_tone_excluded) # participants do not have rt in both struture and random

gender_tone_excludedonly1 <- merge_rt_pvt_cereb_mri$gender[(is.na(merge_rt_pvt_cereb_mri$structured_tone))|(is.na(merge_rt_pvt_cereb_mri$random_tone))]
data.frame(gender_tone_excludedonly1) #participants do not have rt in structure or random

gender_tone_rtinboth <- merge_rt_pvt_cereb_mri$gender[(!is.na(merge_rt_pvt_cereb_mri$structured_tone))&(!is.na(merge_rt_pvt_cereb_mri$random_tone))]
data.frame(gender_tone_rtinboth) #participants have rt in both structure or random


# ---------merge merge_rt_pvt_cereb_mri with cerebellum parcel sig 

cerebellum_sig_and_froi <- read.csv("subjs_in_parcel2_cerebellum.csv")
cereb_sig_participants <- cerebellum_sig_and_froi[ ,c("subject_ID", "sig")]
mri_age_gender <- merge_rt_pvt_cereb_mri[ ,c("subject_ID", "chrono_age", "gender")]
cereb_sig_age_gender=merge(mri_age_gender,cereb_sig_participants,by='subject_ID', all=TRUE, sort=TRUE,suffixes = c("_beh","_cereb"))
# cereb_sig_female <- cereb_sig_age_gender$gender[(cereb_sig_age_gender$sig == 1) & (cereb_sig_age_gender$gender == 0)]
cereb_sig_female <- cereb_sig_age_gender$gender[(cereb_sig_age_gender$sig == 1)]
cereb_unsig_female <- cereb_sig_age_gender$gender[(cereb_sig_age_gender$sig == 0)]
# sum(cereb_sig_age_gender$gender[(cereb_sig_age_gender$sig == 0)])

cereb_sig1 <- cereb_sig_age_gender$subject_ID[(cereb_sig_age_gender$sig == 1)]
cereb_sig0 <- cereb_sig_age_gender$subject_ID[(cereb_sig_age_gender$sig == 0)]









