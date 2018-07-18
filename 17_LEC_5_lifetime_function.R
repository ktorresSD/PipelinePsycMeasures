#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 17, LEC-5 (lifetime)
##########################################################################################
leclife<- function(dat0, exportdate)
{
#Load plyr library
library(plyr)

#Only retain relevant variables
datleclife <- subset(dat0, select= c(assessment_id,vista_lastname, visit_number,
                              LEC_5_1_natdis,
                              LEC_5_2_fire,
                              LEC_5_3_accid,
                              
                              LEC_5_4_seriousacc,
                              LEC_5_5_expos,
                              LEC_5_6_physass,
                              LEC_5_7_assweap,
                              LEC_5_8_sexass,
                              
                              LEC_5_9_otherunw,
                              LEC_5_10_combat,
                              LEC_5_11_captiv,
                              LEC_5_12_life.threat,
                              LEC_5_13_severehum,
                              
                              LEC_5_14_suddviol,
                              LEC_5_15_suddacci,
                              LEC_5_16_seriousinj,
                              LEC_5_17_anyother))

#if any LEC scored 1-4 LEC, then Criteria A is yes

df<- datleclife[,4:19]
crita1<- rowSums(df == 1 | df == 2 |df ==3 | df==4, na.rm = TRUE) > 0L
datleclife$critA1<- as.numeric(crita1)

#________________________________________________________________________________________              
# Completeness check
#----------------------------------------------------------------------------------------

lec_score1 <- function(x)
{
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
  
  
  #checking for completeness
  
  variableshere <- c(LEC_5_1_natdis,
                     LEC_5_2_fire,
                     LEC_5_3_accid,
                     
                     LEC_5_4_seriousacc,
                     LEC_5_5_expos,
                     LEC_5_6_physass,
                     LEC_5_7_assweap,
                     LEC_5_8_sexass,
                     
                     LEC_5_9_otherunw,
                     LEC_5_10_combat,
                     LEC_5_11_captiv,
                     LEC_5_12_life.threat,
                     LEC_5_13_severehum,
                     
                     LEC_5_14_suddviol,
                     LEC_5_15_suddacci,
                     LEC_5_16_seriousinj,
                     LEC_5_17_anyother
           )
  
  
  
  data_complete_lec<- as.numeric(
    sum(
      is.na(
        c( variableshere 
        )
      )
    ) == 0
  )
  
  data_not_attempted_lec<- as.numeric(
    sum(
      is.na(
        c( variableshere 
        )
      )
    ) == 17
  )
  
  completeness_lec1<- "1"
  if(!(is.na(data_not_attempted_lec))){
    if(data_not_attempted_lec==1)
    {
      completeness_lec1 <- "not attempted"}else{}
  }else{completeness_lec1<-NA}
  
  if(!(is.na(data_complete_lec))){
    if(data_complete_lec==1){
      completeness_lec1 <- "complete"} else{}
  }else{completeness_lec1<-NA}
  
  
  if(data_not_attempted_lec==0 & data_complete_lec==0){
    completeness_lec1 <- "partially completed"}else{}
  
  scores <- data.frame(data_complete_lec, data_not_attempted_lec, completeness_lec1)
  
  return(scores)
}


#Calculate summary scores in data 
datalec_scored1 <- adply(datleclife, 1, lec_score1)

#________________________________________________________________________________________ 
#Export
#----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/17_LEC-5_lifetime/LEC-5_lifetime_reduced_data_export_", exportdate, ".csv", sep="")
write.csv( datalec_scored1  , filename,quote=T,row.names=F,na="#N/A")

print("17_LEC_lifetime_NEW_done")


#return completness column
myvars <- c("assessment_id", "completeness_lec1")
newdata <- datalec_scored1[myvars]
return(newdata)
}


