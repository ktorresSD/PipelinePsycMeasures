#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 16, ISI Medication Question
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
datisi2 <- subset(dat0, 
              select= c(assessment_id,vista_lastname,
                        ISI_Medications, ISI_WhatMeds, ISI_numberofdays
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
write.csv( datisi2, "~/Biobank/16_ISI_MedQuestion/ISI_MedQuestion_reduced_data_export_20171221.csv",quote=T,row.names=F,na="#N/A")


