#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 2, BAT-L interval (head injury)
##########################################################################################

#Load plyr library
library(plyr)

#To the user: Set path to where data is stored
setwd("~/Biobank/data")
#________________________________________________________________________________________
#READ AND SUBSET LARGE DATA TO ONLY CONTAIN DESIRED QUESTIONAIRE VARIABLES
#----------------------------------------------------------------------------------------
#Read all data
dat0 <- read.csv('joined_data_export_20171221',header=T,na.strings=c(NA,999))

#Only retain relevant variables
datbatli <- subset(dat0, 
              select= c(assessment_id,vista_lastname,
                        BATL_Int_headinj,BATL_Int_howinj,BATL_timeuncon
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
write.csv(datbatli , "~/Biobank/2_BAT-L_interval/BAT-L_interval_reduced_data_export_20171221.csv",quote=T,row.names=F,na="#N/A")


