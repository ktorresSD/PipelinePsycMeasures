#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 6,basic_pain
##########################################################################################

#Load plyr library
library(plyr)

#To the user: Set path to where data is stored
setwd("~/Biobank/data")
#________________________________________________________________________________________
#READ AND SUBSET LARGE DATA TO ONLY CONTAIN DESIRED QUESTIONAIRE VARIABLES
#----------------------------------------------------------------------------------------
#Read all data
dat0 <- read.csv('joined_data_export_20180606.csv',header=T,na.strings=c(NA,999))

#Only retain relevant variables
datpain <- subset(dat0, select= c(assessment_id,vista_lastname,
                              pain_area_text,
                              pain_area_text_0,
                              pain_area_text_1,
                              pain_area_text_2,
                              pain_area_text_3,
                              pain_area_text_4,
                              pain_area,
                              pain_number,
                              pain_area_count,
                              pain_formula
                              
              ))
#________________________________________________________________________________________
# Data Manipulation and cleaning
#----------------------------------------------------------------------------------------

#________________________________________________________________________________________              
# SCORING Functions Defined
#----------------------------------------------------------------------------------------

datpain$pain_score<-ifelse(datpain$pain_number >=4, TRUE, FALSE)

#________________________________________________________________________________________ 
#Export data
#----------------------------------------------------------------------------------------
write.csv(datpain, "~/Biobank/6_basic_pain/basic_pain_reduced_data_export_20180606.csv",quote=T,row.names=F,na="#N/A")


