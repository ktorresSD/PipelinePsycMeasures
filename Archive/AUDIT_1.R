#########################################################################################
# Last Date modified: 05/14/2018
# Author: Katy Torres
# Description: Subset of question 1, AUDIT
##########################################################################################

#Load plyr library
library(plyr)

#To the user: Set path to where data is stored
setwd("~/Biobank/data")
#________________________________________________________________________________________
#READ AND SUBSET LARGE DATA TO ONLY CONTAIN DESIRED QUESTIONAIRE VARIABLES
#----------------------------------------------------------------------------------------
#Read all data
dat0 <- read.csv('joined_data_export_20180328.csv',header=T,na.strings=c(NA,999))

#Only retain relevant variables
dataudit <- subset(dat0, 
              select= c(assessment_id,vista_lastname,
                        alcol2_many,
                        alcol1_often,
                        alcol3_six,
                        alcol4_often,
                        alcol5_fail,
                        alcol6_start,
                        alcol7_guilt,
                        alcol8_remember,
                        alcol9_injure,
                        alcol10_concern
              ))
#________________________________________________________________________________________
# Data Manipulation and cleaning
#----------------------------------------------------------------------------------------

#recode the question 9 and 10 codes for the purpose of scoring accoring to WHO-AUDIT Guidelines
dataudit$alcol9_injure_RECODE <- dataudit$alcol9_injure
dataudit$alcol9_injure_RECODE[dataudit$alcol9_injure_RECODE==2] <- 4
dataudit$alcol9_injure_RECODE[dataudit$alcol9_injure_RECODE==1] <- 2


dataudit$alcol10_concern_RECODE <- dataudit$alcol10_concer
dataudit$alcol10_concern_RECODE[dataudit$alcol10_concern_RECODE==2] <- 4
dataudit$alcol10_concern_RECODE[dataudit$alcol10_concern_RECODE==1] <- 2


#________________________________________________________________________________________              
# SCORING Functions Defined
#----------------------------------------------------------------------------------------
#Scoring function defined
audit_score <- function(x)
{
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
  
audit_total_score <- alcol1_often + alcol2_many +
  alcol3_six + alcol4_often + alcol5_fail + alcol6_start + alcol7_guilt +
  alcol8_remember + alcol9_injure_RECODE + alcol10_concern_RECODE


data_complete_audit<- as.numeric(
  sum(
    is.na(
      c(alcol2_many,
        alcol1_often,
        alcol3_six,
        alcol4_often,
        alcol5_fail,
        alcol6_start,
        alcol7_guilt,
        alcol8_remember,
        alcol9_injure_RECODE,
        alcol10_concern_RECODE
      )
    )
  ) == 0
)

#sum of all non-na entries
audit_incomplete <- sum(alcol2_many,
                      alcol1_often,
                      alcol3_six,
                      alcol4_often,
                      alcol5_fail,
                      alcol6_start,
                      alcol7_guilt,
                      alcol8_remember,
                      alcol9_injure_RECODE,
                      alcol10_concern_RECODE,na.rm=T)


indicator<- "-4"


#score greater than 8 is an indicator of hazardous and harmful alcohol use, 
#as well as possible alcohol dependence.

if(!(is.na(audit_total_score))){
  if(audit_total_score < 8)
  {
    indicator<-0}else{indicator<-1}
}else{indicator<-NA}
  
    
  scores <- data.frame(audit_total_score, data_complete_audit, audit_incomplete, indicator)
  
  return(scores)
}


#Calculate summary scores in data 
dataudit_scored <- adply(dataudit, 1, audit_score)


#________________________________________________________________________________________ 
#Export data
#----------------------------------------------------------------------------------------
write.csv(dataudit_scored, "~/Biobank/1_AUDIT/AUDIT_reduced_data_export_20180514.csv",quote=T,row.names=F,na="#N/A")


