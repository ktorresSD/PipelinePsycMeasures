#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 5, Basic Demographic and scoring functions
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
datdemo <- subset(dat0, 
              select= c(assessment_id,vista_lastname,
                        demo_gender_r,
                        demo_YOB_r,
                        demo_weight_r,
                        demo_heightft_r,
                        demo_heightinch_r,
                        demo_ethnic_r,
                        
                        demo_racewhite,
                        demo_race_black,
                        demo_race_amind,
                        demo_race_pacisl,
                        demo_race_asian,
                        demo_race_decline,
                        demo_race_oth,
                        
                        demo_relationship_r
                        
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
write.csv( datdemo, "~/Biobank/5_Basic_Demographic/Basic_Demographic_reduced_data_export_20171221.csv",quote=T,row.names=F,na="#N/A")


