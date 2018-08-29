#########################################################################################
# Last datBTBISe modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 11, DRRI_PBE
##########################################################################################

#Load plyr library
 library(plyr)

#To the user: Set path to where data is stored
 setwd("~/Biobank/data")

#Read all data
 dat0 <- read.csv('joined_data_export_20171221.csv',header=T,na.strings=c(NA,999))

#Only retain relevant variables
 datpbe <- subset(dat0, 
               select= c(assessment_id,vista_lastname,
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
#NOTE: For summation purposes we will remove all NA's/treat them as zeros
 datpbe$DRRI_PBE_scores <- as.numeric(rowSums(datpbe[,c("DRRIPBE1",
                                                  "DRRIPBE2",
                                                  "DRRIPBE3",
                                                  "DRRIPBE4",
                                                  "DRRIPBE5",
                                                  "DRRIPBE6",
                                                  "DRRIPBE7",
                                                  "DRRIPBE8",
                                                  "DRRIPBE9",
                                                  "DRRIPBE10",
                                                  "DRRIPBE11",
                                                  "DRRIPBE12",
                                                  "DRRIPBE13")],na.rm=FALSE))
 
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
   
   DRRI_pbe_scores_total_incomplete <- sum(c(DRRIPBE1,
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
                                             DRRIPBE13),na.rm=T)
   
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
   #detach(x)
   scorespbe <- data.frame( DRRI_pbe_scores_total, DRRI_pbe_scores_total_incomplete, data_complete_pbe)
   
   return(scorespbe)
   
 }
 
 #Calculate summary scores in datBTBISa
 score_datpbe <- adply(datpbe, 1, score_pbe)
 
#________________________________________________________________________________________ 
#Export data
 write.csv(  score_datpbe, "~/Biobank/11_DRR12_PBE/DRR12_PBE_reduced_data_export_20171221.csv",quote=T,row.names=F,na="#N/A")


