#########################################################################################
# Last Date modified:12/21/2017
# Author: Katy Torres
# Description: Subset of question 12, Demographic: Education, Employment & Income
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
datemploy <- subset(dat0, 
              select= c(assessment_id,vista_lastname,
                        demo_income_group,
                        demo_education,
                        demo_workstatus,
                        demo_hours,
                        demo_occupation,
                        
                        demo_income_none,
                        demo_income_wrk,
                        demo_income_unemp,
                        demo_income_dis,
                        demo_income_gi,
                        demo_income_retire,
                        demo_income_other,
                        demo_income_spec
                        
                        
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
write.csv( datemploy, "~/Biobank/12_DEMO/Demographic_employment_reduced_data_export_20171221.csv",quote=T,row.names=F,na="#N/A")


