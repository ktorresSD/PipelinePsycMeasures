#########################################################################################
# Last datBTBISe modified: 06/18/2018
# Author: Katy Torres
# Description: Subset of question 10, DRRI_CES
##########################################################################################
drrices <- function(dat0, exportdate)
{

#Load plyr library
 library(plyr)

  dat<-dat0[dat0$visit_number==1,]
#Only retain relevant variables
 datces <- subset(dat, 
               select= c(assessment_id,vista_lastname,visit_number,
                         DRRICE1,
                         DRRICE2,
                         DRRICE3,
                         DRRICE4,
                         DRRICE5,
                         DRRICE6,
                         DRRICE7,
                         DRRICE8,
                         DRRICE9,
                         DRRICE10,
                         DRRICE11,
                         DRRICE12,
                         DRRICE13,
                         DRRICE14,
                         DRRICE15,
                         DRRICE16,
                         DRRICE17
                         
               ))
#________________________________________________________________________________________              
#Scoring function
#----------------------------------------------------------------------------------------
#Calculate summary scores in data
 score_ces <- function(x)
 {
   
   #attach(x)
   for (v in 1:length(x)) assign(names(x)[v], x[[v]])
   
   DRRI_CES_scores_total<- sum(c(DRRICE1,
                                             DRRICE2,
                                             DRRICE3,
                                             DRRICE4,
                                             DRRICE5,
                                             DRRICE6,
                                             DRRICE7,
                                             DRRICE8,
                                             DRRICE9,
                                             DRRICE10,
                                             DRRICE11,
                                             DRRICE12,
                                             DRRICE13,
                                             DRRICE14,
                                             DRRICE15,
                                             DRRICE16,
                                             DRRICE17),na.rm=F)

   data_complete_ces<- as.numeric(
     sum(
       is.na(
         c(DRRICE1,
           DRRICE2,
           DRRICE3,
           DRRICE4,
           DRRICE5,
           DRRICE6,
           DRRICE7,
           DRRICE8,
           DRRICE9,
           DRRICE10,
           DRRICE11,
           DRRICE12,
           DRRICE13,
           DRRICE14,
           DRRICE15,
           DRRICE16,
           DRRICE17
         )
       )
     ) == 0
   )
   
   data_not_attempted_ces<- as.numeric(
     sum(
       is.na(
         c(DRRICE1,
           DRRICE2,
           DRRICE3,
           DRRICE4,
           DRRICE5,
           DRRICE6,
           DRRICE7,
           DRRICE8,
           DRRICE9,
           DRRICE10,
           DRRICE11,
           DRRICE12,
           DRRICE13,
           DRRICE14,
           DRRICE15,
           DRRICE16,
           DRRICE17
         )
       )
     ) == 17
   )
   
   completeness_ces<- "1"
   if(!(is.na(data_not_attempted_ces))){
     if(data_not_attempted_ces==1)
     {
       completeness_ces <- "not attempted"}else{}
   }else{completeness_ces<-NA}
   
   if(!(is.na(data_complete_ces))){
     if(data_complete_ces==1){
       completeness_ces <- "complete"} else{}
   }else{completeness_ces<-NA}
   
   
   if(data_not_attempted_ces==0 & data_complete_ces==0){
     completeness_ces <- "partially completed"}else{}
   
   #detach(x)
   scoresces <- data.frame( DRRI_CES_scores_total, data_not_attempted_ces, data_complete_ces, completeness_ces)
   
   return(scoresces)
  
 }
 
 #Calculate summary scores in datBTBISa
 score_datces <- adply(datces, 1, score_ces)
 
 #to anonymize data
 score_datces1<- within(score_datces ,
                           {
                             assessment_id <- NULL
                             vista_lastname <- NULL
                           })
 
 
 
 
 
 
 #completeness table
 table(score_datces$completeness_ces)
 
 library(psych)
         
 #summary statistics for total PCL
 describe(score_datces$DRRI_CES_scores_total)
 
 #mode
 Mode <- function(x) {
   ux <- unique(x)
   ux[which.max(tabulate(match(x, ux)))]
 }
 
 Mode(score_datces$DRRI_CES_scores_total)
 
 hist(score_datces$DRRI_CES_scores_total, ylim=c(0,50), xlim = c(10,102),
      xlab = "Total DRRI_CES Score", col = c("steelblue3"), main = "Histogram for Total DRRI_CES Score")

 
 
 
 #________________________________________________________________________________________ 
 #Export
 #----------------------------------------------------------------------------------------
 filename <- paste("~/Biobank/10_DRRI2_CES/DRRICES_scored_data_export.csv", sep="")
 write.csv(score_datces, filename,quote=T,row.names=F,na="NA")
 
 filename <- paste("~/Biobank/10_DRRI2_CES/DRRICES_scored_data_export_DEIDENTIFIED.csv", sep="")
 write.csv(score_datces1, filename,quote=T,row.names=F,na="NA")
print("10_DRRI_CES_done")

#return completness column
myvars <- c("assessment_id", "completeness_ces")
newdata <- score_datces[myvars]
return(newdata)
}
