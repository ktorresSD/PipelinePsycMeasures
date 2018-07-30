#########################################################################################
# Last datBTBISe modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 11, DRRI_PBE
##########################################################################################

drripbe <- function(dat0, exportdate)
{
  dat1<-dat0[dat0$visit_number==1,]
#Load plyr library
 library(plyr)

#Only retain relevant variables
 datpbe <- subset(dat1, 
               select= c(assessment_id,vista_lastname, visit_number,
                         DRRIPBE1,
                         DRRIPBE2,
                         DRRIPBE3,
                         DRRIPBE4,
                         DRRIPBE5,
                         DRRIPBE6,
                         DRRIPBE7,
                         DRRIPBE8,
                         DRRIPBE9,
                         DRRIPBE10,
                         DRRIPBE11,
                         DRRIPBE12,
                         DRRIPBE13
               ))
#________________________________________________________________________________________              
#Scoring function
#----------------------------------------------------------------------------------------
#Calculate summary scores in data
 
 score_pbe <- function(x)
 {
   
   #attach(x)
   for (v in 1:length(x)) assign(names(x)[v], x[[v]])
   
   DRRI_pbe_scores_total<- sum(c(DRRIPBE1,
                                 DRRIPBE2,
                                 DRRIPBE3,
                                 DRRIPBE4,
                                 DRRIPBE5,
                                 DRRIPBE6,
                                 DRRIPBE7,
                                 DRRIPBE8,
                                 DRRIPBE9,
                                 DRRIPBE10,
                                 DRRIPBE11,
                                 DRRIPBE12,
                                 DRRIPBE13),na.rm=F)
   
   data_complete_pbe<- as.numeric(
     sum(
       is.na(
         c(DRRIPBE1,
           DRRIPBE2,
           DRRIPBE3,
           DRRIPBE4,
           DRRIPBE5,
           DRRIPBE6,
           DRRIPBE7,
           DRRIPBE8,
           DRRIPBE9,
           DRRIPBE10,
           DRRIPBE11,
           DRRIPBE12,
           DRRIPBE13
         )
       )
     ) == 0
   )
   
   data_not_attempted_pbe<- as.numeric(
     sum(
       is.na(
         c(DRRIPBE1,
           DRRIPBE2,
           DRRIPBE3,
           DRRIPBE4,
           DRRIPBE5,
           DRRIPBE6,
           DRRIPBE7,
           DRRIPBE8,
           DRRIPBE9,
           DRRIPBE10,
           DRRIPBE11,
           DRRIPBE12,
           DRRIPBE13
         )
       )
     ) == 13
   )
   
   completeness_pbe<- "1"
   if(!(is.na(data_not_attempted_pbe))){
     if(data_not_attempted_pbe==1)
     {
       completeness_pbe <- "not attempted"}else{}
   }else{completeness_pbe<-NA}
   
   if(!(is.na(data_complete_pbe))){
     if(data_complete_pbe==1){
       completeness_pbe <- "complete"} else{}
   }else{completeness_pbe<-NA}
   
   
   if(data_not_attempted_pbe==0 & data_complete_pbe==0){
     completeness_pbe <- "partially completed"}else{}
   
   #detach(x)
   scorespbe <- data.frame( DRRI_pbe_scores_total, data_complete_pbe, data_not_attempted_pbe, completeness_pbe)
   
   return(scorespbe)
   
 }
 
 #Calculate summary scores in datBTBISa
 score_datpbe <- adply(datpbe, 1, score_pbe)
 
 #________________________________________________________________________________________ 
 #Export
 #----------------------------------------------------------------------------------------
 filename <- paste("~/Biobank/11_DRR12_PBE/DRR12_PBE_reduced_data_export_", exportdate, ".csv", sep="")
 write.csv(score_datpbe, filename,quote=T,row.names=F,na="#N/A")
 
print("11_DRRI_PBE_done")
 
#return completness column
myvars <- c("assessment_id", "completeness_pbe")
newdata <- score_datpbe[myvars]
return(newdata)
}
