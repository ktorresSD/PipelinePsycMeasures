#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 7, Brief CESAMH Biorepository Sample Status Survey
##########################################################################################

#Load plyr library
library(plyr)

#To the user: Set path to where data is stored
setwd("~/Biobank/data")
#________________________________________________________________________________________
#READ AND SUBSET LARGE DATA TO ONLY CONTAIN DESIRED QUESTIONAIRE VARIABLES
#----------------------------------------------------------------------------------------
#Read all data
dat0 <- read.csv('joined_data_export_20171221.csv',header=T,na.strings=c(NA,999))

#Only retain relevant variables
datcesamh<- subset(dat0, 
              select= c(assessment_id,vista_lastname,
                        Stat1_cold,
                        Stat2_nic,
                        
                        Stat2_nicroute_chew,
                        Stat2_nicroute_cig,
                        Stat2_nicroute_vap,
                        Stat2_nicroute_inh,
                        Stat2_nicroute_pat,
                        Stat2_nictime,
                        Stat3_eat,
                        Stat4_caff,
                        Stat5_alc,
                        Stat6_diet,
                        
                        
                        Stat7_med_blood,
                        Stat7_med_statin,
                        Stat7_med_heart,
                        Stat7_med_diabetes,
                        Stat7_med_psych,
                        Stat7_med_pain,
                        Stat7_med_sleep,
                        Stat7_med_antibiotics,
                        Stat7_med_prost,
                        Stat7_med_allerg,
                        Stat7_med_steroid,
                        
                        Stat8_marijtoday,
                        Stat9_cannabisweek,
                        Stat10_bed_night,
                        Stat10_bed_morn,
                        Stat10_bed_wakeupnight
                        
              ))
#________________________________________________________________________________________
# Data Manipulation and cleaning
#----------------------------------------------------------------------------------------

#________________________________________________________________________________________              
# SCORING Functions Defined
#----------------------------------------------------------------------------------------


#________________________________________________________________________________________ 
#Export data
#----------------------------------------------------------------------------------------
write.csv( datcesamh, "~/Biobank/7_Brief_CESAMH_Biorepository_Survey/Brief_CESAMH_reduced_data_export_20171221.csv",quote=T,row.names=F,na="#N/A")


