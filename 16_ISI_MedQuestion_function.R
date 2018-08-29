#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 16, ISI Medication Question
##########################################################################################
isi2<- function(dat0, exportdate)
{

  
#Only retain relevant variables
datisi2 <- subset(dat0, 
              select= c(assessment_id,vista_lastname,visit_number,
                        ISI_Medications, ISI_WhatMeds, ISI_numberofdays
              ))

isimed_score <- function(x)
{
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
  
  if(!(is.na(ISI_Medications))){
    if(ISI_Medications==0)
    {
      meds<- 0}else{meds <- 1}
  }else{meds<-NA}
  
  
  #checking for completeness
  data_complete_isimed<- as.numeric(
    sum(
      is.na(
        c(ISI_Medications, ISI_WhatMeds, ISI_numberofdays
        )
      )
    ) == 0
  )
  
  data_not_attempted_isimed<- as.numeric(
    sum(
      is.na(
        c(ISI_Medications, ISI_WhatMeds, ISI_numberofdays
        )
      )
    ) == 3
  )
  
  completeness_isimed<- "1"
  if(!(is.na(data_not_attempted_isimed))){
    if(data_not_attempted_isimed==1)
    {
      completeness_isimed <- "not attempted"}else{}
  }else{completeness_isimed<-NA}
  
  if(!(is.na(data_complete_isimed))){
    if(data_complete_isimed==1){
      completeness_isimed <- "complete"} else{}
  }else{completeness_isimed<-NA}
  
  if(!(is.na(meds))){
    if(meds==0){
      completeness_isimed <- "complete"} else{}
  }else{completeness_isimed<-NA}
  
  
  if(data_not_attempted_isimed==0 & data_complete_isimed==0 & meds==1){
    completeness_isimed <- "partially completed"}else{}
  
  scores <- data.frame(completeness_isimed )
  
  return(scores)
}

datisimed_scored <- adply(datisi2, 1, isimed_score)

#to anonymize data
datisimed_scored1<- within(datisimed_scored,
                          {
                            assessment_id <- NULL
                            vista_lastname <- NULL
                          })

#________________________________________________________________________________________ 
#Export
#----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/16_ISI_MedQuestion/ISI_MedQuestion_scored_data_export.csv", sep="")
write.csv( datisimed_scored, filename,quote=T,row.names=F,na="#N/A")

filename <- paste("~/Biobank/16_ISI_MedQuestion/ISI_MedQuestion_scored_data_export_FEIDENTIFIED.csv", sep="")
write.csv( datisimed_scored1, filename,quote=T,row.names=F,na="#N/A")

print("16_ISI_MED_done")


#return completness column
myvars <- c("assessment_id", "completeness_isimed")
newdata <- datisimed_scored[myvars]
return(newdata)
}


