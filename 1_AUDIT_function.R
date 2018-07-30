#########################################################################################
# Last Date modified: 07/03/2018
# Author: Katy Torres
# Description: Subset of question 1, AUDIT
##########################################################################################
audit <- function(dat0, exportdate)
{
#Load plyr library
library(plyr)

#Only retain relevant variables
dataudit <- subset(dat0, 
              select= c(assessment_id,vista_lastname,visit_number,
                        alcol1_often,
                        alcol2_many,
                        alcol3_six,
                        alcol4_often,
                        alcol5_fail,
                        alcol6_start,
                        alcol7_guilt,
                        alcol8_remember,
                        alcol9_injure,
                        alcol10_concern,
                        audit10_score,
                        audit.c_score
                        
              ))
#________________________________________________________________________________________
# Data Manipulation and cleaning
#----------------------------------------------------------------------------------------


#recode the question 9 and 10 codes for the purpose of scoring accoring to WHO-AUDIT Guidelines
dataudit$alcol9_injure_RECODE <- dataudit$alcol9_injure
dataudit$alcol9_injure_RECODE[dataudit$alcol9_injure_RECODE==2] <- 4
dataudit$alcol9_injure_RECODE[dataudit$alcol9_injure_RECODE==1] <- 2


dataudit$alcol10_concern_RECODE <- dataudit$alcol10_concern
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


if(!(is.na(alcol1_often))){
  if(alcol1_often==0)
  {
    drinker <- 0}else{drinker <- 1}
}else{drinker<-NA}


#intervention variable by risk level
intervention<- "-4"
if(!(is.na(audit_total_score))){
  if(audit_total_score<=7){intervention <- "Alcohol Education"}
  else if(audit_total_score >= 8 & audit_total_score <= 15){intervention <- "Simple Advice"}
  else if(audit_total_score >= 16 & audit_total_score <= 19){intervention <- "Brief Counseling and Monitoring"}
  else if(audit_total_score >=20){intervention <- "Referral to Specialist"}
}else{intervention<-"NA"}

indicator<- "-4"
#score greater than 8 is an indicator of hazardous and harmful alcohol use,
#as well as possible alcohol dependence. Only checks if all 10 questions are answered
  if(!(is.na(audit_total_score))){
    if(audit_total_score < 8)
    {
      indicator<-0}else{indicator<-1}
  }else{indicator<-NA}
#________________________________________________________________________________________              
# Completeness check
#----------------------------------------------------------------------------------------

#checking for completeness
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

data_not_attempted_audit<- as.numeric(
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
  ) == 10
)

completeness_audit<- "1"
if(!(is.na(data_not_attempted_audit))){
  if(data_not_attempted_audit==1)
  {
    completeness_audit <- "not attempted"}else{}
}else{completeness_audit<-NA}

if(!(is.na(data_complete_audit))){
  if(data_complete_audit==1){
    completeness_audit <- "complete"} else{}
}else{}

if(!(is.na(drinker))){
  if(drinker==0){
    completeness_audit <- "complete"} else{}
}else{}


  if(data_not_attempted_audit==0 & data_complete_audit==0 & drinker==1){
    completeness_audit <- "partially completed"}else{}
    
  scores <- data.frame(drinker,audit_total_score, indicator, intervention, data_complete_audit, data_not_attempted_audit, completeness_audit )
  
  return(scores)
}


#Calculate summary scores in data 
dataudit_scored <- adply(dataudit, 1, audit_score)

#________________________________________________________________________________________ 
#Export data
#----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/1_AUDIT/AUDIT_reduced_data_export_", exportdate, ".csv", sep="")
write.csv(dataudit_scored, filename ,quote=T,row.names=F,na="#N/A")


print("1_Audit_done")

#return completness column
myvars <- c("assessment_id", "completeness_audit")
newdata <- dataudit_scored[myvars]
return(newdata)
}




