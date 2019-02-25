#########################################################################################
# Last Date modified: 10/22/2018
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
  }else{}
  
  if(!(is.na(data_complete_isimed))){
    if(data_complete_isimed==1){
      completeness_isimed <- "complete"} else{}
  }else{}
  
  if(!(is.na(meds))){
    if(meds==0){
      completeness_isimed <- "complete"} else{}
  }else{}
  
  
  if(data_not_attempted_isimed==0 & data_complete_isimed==0 & meds==1){
    completeness_isimed <- "partially completed"}else{}
  
  scores <- data.frame(completeness_isimed )
  
  return(scores)
}

insomnia_scores <- adply(datisi2, 1, isimed_score)

#to anonymize data
insomnia_scores1<- within(insomnia_scores,
                          {
                            assessment_id <- NULL
                            vista_lastname <- NULL
                          })


# #________________________________________________________________________________________              
# # Descriptive Stats and plots for report
# #----------------------------------------------------------------------------------------
# library(psych)
# 
# #subset by visit to get report information
# v1 <- insomnia_scores[ which(insomnia_scores$visit_number==1), ]
# v2 <- insomnia_scores[ which(insomnia_scores$visit_number==2), ]
# v3 <- insomnia_scores[ which(insomnia_scores$visit_number==3), ]
# 
# #completeness table
# table(insomnia_scores$completeness_isimed, insomnia_scores$visit_number)
# 
# summary statistics for total PCL
# describe(v1$insomnia_total)
# describe(v2$insomnia_total)
# describe(v3$insomnia_total)
# describe(insomnia_scores$insomnia_total)
# 
# #mode
# Mode <- function(x) {
#   ux <- unique(x)
#   ux[which.max(tabulate(match(x, ux)))]
# }
# 
# Mode(v1$insomnia_total)
# Mode(v2$insomnia_total)
# Mode(v3$insomnia_total)
# Mode(insomnia_scores$insomnia_total)
# 
# 
# #histograms
# par(mfrow=c(2,2))
# hist(insomnia_scores$insomnia_total, breaks=10, xlab = "ISI Score", ylim=c(0,45), col = c("lightyellow"), main = "ISI total Score (all visits)")
# hist(v1$insomnia_total, breaks=10, xlab = "ISI Score", ylim=c(0,45), col = c("lightyellow"), main = "ISI total Score (visit 1 only)")
# hist(v2$insomnia_total, breaks=10, xlab = "ISI Score", ylim=c(0,45), col = c("lightyellow"), main = "ISI total Score (visit 2 only)")
# hist(v3$insomnia_total, breaks=10, xlab = "ISI Score", ylim=c(0,45), col = c("lightyellow"), main = "ISI total Score (visit 3 only)")
# 
# 
# par(mfrow=c(1,1))
# hist(insomnia_scores$insomnia_total, breaks=17, xlim=c(0,27), xlab = "Total ISI Score", col = c("steelblue3"), main = "Total ISI Score")
# abline(v = 15, lty = 2, lwd=2)
# axis(1, 1:28)
# legend('topright',legend=c("Clinical Insomnia"),lty = 2, lwd=2)
# 
# 
# 
# 
# barplot(table(insomnia_scores$score_interpretation_isi),
#         col = c("mistyrose" ,"lavender", "lightblue", "lightskyblue3"),
#         main = "Count of subjects in each ISI Diagnosis category",
#         ylab = "Subject Count",
#         names.arg = c("Not clinically sig", "Subtreshold", "Moderate severity", "Severe" ))
# abline(col= "slategray", v=2.50, lwd= 2, lty = "dashed")
# mtext("                               Clinical insomnia", col= "slategray")




#________________________________________________________________________________________ 
#Export
#----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/16_ISI_MedQuestion/ISI_MedQuestion_scored_data_export.csv", sep="")
write.csv( insomnia_scores, filename,quote=T,row.names=F,na="#N/A")

filename <- paste("~/Biobank/16_ISI_MedQuestion/ISI_MedQuestion_scored_data_export_DEIDENTIFIED.csv", sep="")
write.csv( insomnia_scores1, filename,quote=T,row.names=F,na="#N/A")

print("16_ISI_MED_done")


#return completness column
myvars <- c("assessment_id", "completeness_isimed")
newdata <- insomnia_scores[myvars]
return(newdata)
}


