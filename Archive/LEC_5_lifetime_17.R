#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 17, LEC-5 (lifetime)
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
datleclife <- subset(dat0, select= c(assessment_id,vista_lastname,
                              LEC_5_1_natdis,
                              LEC_5_2_fire,
                              LEC_5_3_accid,
                              
                              LEC_5_4_seriousacc,
                              LEC_5_5_expos,
                              LEC_5_6_physass,
                              LEC_5_7_assweap,
                              LEC_5_8_sexass,
                              
                              LEC_5_9_otherunw,
                              LEC_5_10_combat,
                              LEC_5_11_captiv,
                              LEC_5_12_life.threat,
                              LEC_5_13_severehum,
                              
                              LEC_5_14_suddviol,
                              LEC_5_15_suddacci,
                              LEC_5_16_seriousinj,
                              LEC_5_17_anyother
                        
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
write.csv( datleclife, "~/Biobank/17_LEC-5_lifetime/LEC-5_lifetime_reduced_data_export_20171221.csv",quote=T,row.names=F,na="#N/A")


