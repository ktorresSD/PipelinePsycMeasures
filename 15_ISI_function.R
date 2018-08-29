#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 15, ISI
##########################################################################################

isi<- function(dat0, exportdate)
{
  
#Load plyr library
library(plyr)
  
#Only retain relevant variables
datisi <- subset(dat0, 
              select= c(assessment_id,vista_lastname,visit_number,
                        sleep1a_falling,
                        sleep1b_staying,
                        sleep1c_waking,
                        sleep2_satisfied,
                        sleep3_interfere,
                        sleep4_noticeable,
                        sleep5_worried,
                        sleep_score
              ))
#________________________________________________________________________________________
# Data Manipulation and cleaning
#----------------------------------------------------------------------------------------

#________________________________________________________________________________________              
# SCORING Functions Defined
#----------------------------------------------------------------------------------------
#Scoring function defined
insomnia <- function(x)
{

  #attach(x)
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
  
  #summary score is the summation of items 1-7
  #Note: This function is not designed to handle NA values (subject must have complete data)
  
  
  insomnia_total <-   sleep1a_falling+
  sleep1b_staying+
  sleep1c_waking+
  sleep2_satisfied+
  sleep3_interfere+
  sleep4_noticeable+
  sleep5_worried+
  sleep_score
  
  
  not_clinically_significant_insomnia <- as.numeric(insomnia_total <= 7)
  subthreshold_insomnia <- as.numeric(insomnia_total >= 8 & insomnia_total <= 14)
  moderate_severity_insomnia <- as.numeric(insomnia_total >= 15 & insomnia_total <= 21)
  severe_insomnia <- as.numeric(insomnia_total >= 22)
  
  score_interpretation_isi<- "na"
  if(!(is.na(not_clinically_significant_insomnia))){
    if(not_clinically_significant_insomnia==1)
    {
      score_interpretation_isi <- "not_clinically_significant_insomnia"}else{}
  }else{score_interpretation_isi<-NA}
  
  if(!(is.na(subthreshold_insomnia))){
    if(subthreshold_insomnia==1){
      score_interpretation_isi <- "subthreshold_insomnia"} else{}
  }else{score_interpretation_isi<-NA}
  
  
  if(!(is.na(moderate_severity_insomnia))){
    if(moderate_severity_insomnia==1){
      score_interpretation_isi <- "moderate_severity_insomnia"} else{}
  }else{score_interpretation_isi<-NA}
  
  if(!(is.na(severe_insomnia))){
    if(severe_insomnia==1){
      score_interpretation_isi <- "severe_insomnia"}else{}
  }else{score_interpretation_isi<-NA}
  
  
  
  
  #completeness checks

  data_complete_isi <- as.numeric(
    sum(
      is.na(
        c(sleep1a_falling,
          sleep1b_staying,
          sleep1c_waking,
          sleep2_satisfied,
          sleep3_interfere,
          sleep4_noticeable,
          sleep5_worried,
          sleep_score
        )
      )
    ) == 0
  )
  
  data_not_attempted_isi<- as.numeric(
    sum(
      is.na(
        c(sleep1a_falling,
          sleep1b_staying,
          sleep1c_waking,
          sleep2_satisfied,
          sleep3_interfere,
          sleep4_noticeable,
          sleep5_worried,
          sleep_score
        )
      )
    ) == 8
  )
  
  completeness_isi<- "1"
  if(!(is.na(data_not_attempted_isi))){
    if(data_not_attempted_isi==1)
    {
      completeness_isi <- "not attempted"}else{}
  }else{completeness_isi<-NA}
  
  if(!(is.na(data_complete_isi))){
    if(data_complete_isi==1){
      completeness_isi <- "complete"} else{}
  }else{completeness_isi<-NA}
  
  if(data_not_attempted_isi==0 & data_complete_isi==0){
    completeness_isi <- "partially completed"}else{}
  
  scoresisi <- data.frame(insomnia_total, not_clinically_significant_insomnia,  subthreshold_insomnia, moderate_severity_insomnia, severe_insomnia, score_interpretation_isi, data_not_attempted_isi, data_complete_isi, completeness_isi)
  
  return(scoresisi)
}


#Calculate summary scores in data
insomnia_scores <- adply(datisi, 1, insomnia)

#to anonymize data
insomnia_scores1<- within(insomnia_scores,
                        {
                          assessment_id <- NULL
                          vista_lastname <- NULL
                        })

#________________________________________________________________________________________ 
#Export
#----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/15_ISI/ISI_scored_data_export.csv", sep="")
write.csv( insomnia_scores, filename,quote=T,row.names=F,na="#N/A")

filename <- paste("~/Biobank/15_ISI/ISI_scored_data_export_DEIDENTIFIED.csv", sep="")
write.csv( insomnia_scores1, filename,quote=T,row.names=F,na="#N/A")

print("15_ISI_done")

#return completness column
myvars <- c("assessment_id", "completeness_isi")
newdata <- insomnia_scores[myvars]
return(newdata)
}



