#########################################################################################
# Last datBTBISe modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 10, DRRI_CES
##########################################################################################


#Load plyr library
 library(plyr)

#To the user: Set path to where data is stored
 setwd("~/Biobank/data")

#Read all data
 dat0 <- read.csv('joined_data_export_20171221.csv',header=T,na.strings=c(NA,999))

#Only retain relevant variables
 datces <- subset(dat0, 
               select= c(assessment_id,vista_lastname,
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

   DRRI_CES_scores_total_incomplete <- sum(c(DRRICE1,
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
                                   DRRICE17),na.rm=T)

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
   #detach(x)
   scoresces <- data.frame( DRRI_CES_scores_total, DRRI_CES_scores_total_incomplete, data_complete_ces)
   
   return(scoresces)
  
 }
 
 #Calculate summary scores in datBTBISa
 score_datces <- adply(datces, 1, score_ces)
 
#________________________________________________________________________________________ 
#Export data
 write.csv( score_datces, "~/Biobank/10_DRRI2_CES/DRRICES_reduced_data_export_20171221.csv",quote=T,row.names=F,na="#N/A")


