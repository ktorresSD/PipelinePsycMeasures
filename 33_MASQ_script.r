#########################################################################################
# Last Date modified: 05/17/2018
# Author: Katy Torres
# Description: MASQ scoring function - paper form questionnaire
##########################################################################################

masq<- function(dat0, exportdate)
{
  #Load libraries
  library(plyr)
  library(ggplot2)
  
  #Only retain relevant variables
  datcesamh<- subset(dat0, 
                     select= c(assessment_id,vista_lastname,visit_number,
                               MASQ1_cheerful,
                               MASQ2_optim,
                               MASQ3_happy,
                               MASQ4_proud,
                               MASQ5_unatt,
                               MASQ6_fun,
                               MASQ7_withdra,
                               MASQ8_slow,
                               MASQ9_energy,
                               MASQ10_up,
                               MASQ11_bored,
                               MASQ12_forward,
                               MASQ13_todo,
                               MASQ14_accompl,
                               MASQ15_effort,
                               MASQ16_enjoy,
                               MASQ17_lookforw,
                               MASQ18_hopeful,
                               MASQ19_interest,
                               MASQ20_quick,
                               MASQ21_feltgood,
                               MASQ22_suicide))

#________________________________________________________________________________________              
# SCORING Functions Defined
#----------------------------------------------------------------------------------------
#Scoring function defined
MASQ_score <- function(x)
{
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
  #Negative keyed VARABLES
  cheerful<- 6 -  MASQ1_cheerful
  optimistic<- 6 -  MASQ2_optim
  happy<- 6 -  MASQ3_happy
  proud<- 6 - MASQ4_proud
  fun<- 6 - MASQ6_fun
  energy<- 6 - MASQ9_energy
  up<- 6 - MASQ10_up
  enjoyment<- 6 - MASQ12_forward
  interesting<- 6 - MASQ13_todo
  accomplished<- 6 - MASQ14_accompl
  lookforward<- 6 - MASQ17_lookforw
  future<- 6 - MASQ18_hopeful
  quickly<- 6 - MASQ20_quick
  good<- 6 - MASQ21_feltgood
  
  AD <- cheerful + optimistic + happy + proud + fun + up + enjoyment + interesting + energy +
  accomplished + lookforward + future + quickly + good + MASQ5_unatt + MASQ7_withdra + 
  MASQ8_slow + MASQ11_bored + MASQ15_effort + MASQ16_enjoy + MASQ19_interest + MASQ22_suicide 

  data_complete_MASQ<- as.numeric(
    sum(
      is.na(
        c(cheerful , optimistic , happy , proud , fun , up , enjoyment , interesting , energy,
          accomplished , lookforward , future , quickly , good , MASQ5_unatt , MASQ7_withdra , 
          MASQ8_slow , MASQ11_bored , MASQ15_effort , MASQ16_enjoy , MASQ19_interest , MASQ22_suicide
        ))) == 0)
  
  
  data_not_attempted_MASQ<- as.numeric(
    sum(
      is.na(
        c(cheerful , optimistic , happy , proud , fun , up , enjoyment , interesting , energy,
          accomplished , lookforward , future , quickly , good , MASQ5_unatt , MASQ7_withdra , 
          MASQ8_slow , MASQ11_bored , MASQ15_effort , MASQ16_enjoy , MASQ19_interest , MASQ22_suicide
        ))) == 22)
  
  #________________________________________________________________________________________              
  # Completeness Functions Defined
  #----------------------------------------------------------------------------------------
  
  completeness_MASQ<- "1"
  if(!(is.na(data_not_attempted_MASQ))){
    if(data_not_attempted_MASQ==1)
    {
      completeness_MASQ <- "not attempted"}else{}
  }else{completeness_MASQ<-NA}
  
  if(!(is.na(data_complete_MASQ))){
    if(data_complete_MASQ==1){
      completeness_MASQ <- "complete"} else{}
  }else{completeness_MASQ<-NA}
  
  
  if(!(is.na(data_complete_MASQ))){
    if(data_not_attempted_MASQ == 0 & data_complete_MASQ==0){
      completeness_MASQ <- "partially completed"}else{}
  }else{}
  
  
 scores <- data.frame(AD, data_complete_MASQ, data_not_attempted_MASQ, completeness_MASQ)

return(scores)
}

  
  #Calculate summary scores in data 
  datMASQ_scored <- adply(datcesamh, 1, MASQ_score)
  
  #to anonymize data
  datMASQ_scored1<- within(datMASQ_scored,
                             {
                               assessment_id <- NULL
                               vista_lastname <- NULL
                             })

#________________________________________________________________________________________              
# Completeness Functions Defined
#----------------------------------------------------------------------------------------

# #________________________________________________________________________________________ 
# #Export data
# #----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/33_MASQ/MASQ_reduced_data_export.csv", sep="")
write.csv(datMASQ_scored, filename,quote=T,row.names=F,na="#N/A")

filename <- paste("~/Biobank/33_MASQ/MASQ_reduced_data_export_DEIDENTIFIED.csv", sep="")
write.csv(datMASQ_scored1, filename,quote=T,row.names=F,na="#N/A")

print("33_MASQ_done")

#return completness column
myvars <- c("assessment_id", "completeness_MASQ")
newdata <- datMASQ_scored[myvars]
return(newdata)
}

