#########################################################################################
# Last Date modified: 5/15/2018
# Author: Katy Torres
# Description: Subset of question 23, PHQ9
##########################################################################################

#Load plyr library
 library(plyr)

#To the user: Set path to where data is stored
setwd("~/Biobank/data")


#Read all data
 dat0 <- read.csv('joined_data_export_20180328.csv',header=T,na.strings=c(NA,999))

#Only retain relevant variables
 datphq9 <- subset(dat0, 
               select= c(assessment_id,vista_lastname,
               dep1_interest,
               dep2_down,
               dep3_sleep,
               dep4_tired,
               dep5_appetite,
               dep6_feelbad,
               dep7_concentrate,
               dep8_moveslow,
               dep9_dead,
               dep10_difficult,
               dep_score_phq9))
              
#Scoring function defined
phq9 <- function(x)
{
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
    
	#PHQ-9 summary score is the summation of items 1-9 (not 10)
	#Note: This function is not designed to handle NA values (subject must have complete data)

  #total score              
	phq9_total <- dep1_interest +
	           dep2_down +
               dep3_sleep +
               dep4_tired +
               dep5_appetite +
               dep6_feelbad +
               dep7_concentrate +
               dep8_moveslow +
               dep9_dead
        

	
   #Diagnosis based on score, one diagnosis per column 
	
   minimal_depression <- as.numeric(phq9_total <= 4)
  
   mild_depression <- as.numeric(phq9_total >= 5 & phq9_total <= 9)
   
   moderate_depression <- as.numeric(phq9_total >= 10 & phq9_total <= 14)
   
   moderately_severe_depression <- as.numeric(phq9_total >= 15 & phq9_total <= 19)
   
   severe_depression <- as.numeric(phq9_total >= 20)
   
   
   
   #One column for Diagnosis based on score
   
   if(!(is.na(minimal_depression))){
     if(minimal_depression==1)
     {
       score_interpretation_phq9 <- 1}else{}
   }else{score_interpretation_phq9 <- NA}
   
   if(!(is.na(mild_depression))){
     if(mild_depression==1){
       score_interpretation_phq9 <- 2} else{}
   }else{score_interpretation_phq9 <-NA}

   if(!(is.na(moderate_depression))){
     if(moderate_depression==1){
       score_interpretation_phq9 <- 3}else{}
   }else{score_interpretation_phq9 <-NA}
   
   if(!(is.na(moderately_severe_depression))){
     if(moderately_severe_depression==1){
       score_interpretation_phq9 <- 4}else{}
   }else{score_interpretation_phq9 <-NA}
   
   if(!(is.na(severe_depression))){
     if(severe_depression==1){
       score_interpretation_phq9 <- 5}else{}
   }else{score_interpretation_phq9 <-NA}
   
   

   #score if any items are missing
   phq9_incomplete <- sum(c(dep1_interest ,
	           dep2_down ,
               dep3_sleep ,
               dep4_tired ,
               dep5_appetite ,              
               dep6_feelbad ,
               dep7_concentrate ,
               dep8_moveslow ,
               dep9_dead),na.rm=T)
   
   
   
   #treatment action if total score is greater than 14
     
     if (is.na(phq9_total))
   {
     phq9_treatment <- NA
   } else if (phq9_total <= 14)
   {
     phq9_treatment <- 0
   } else if (phq9_total > 14)
   {
     phq9_treatment <- 1
   }
   
                
    #flag if all entires are complete. 0 if incomplete data
    data_complete_phq9 <- as.numeric( 
                sum(
                    is.na(
                    c(dep1_interest, dep2_down, dep3_sleep, dep4_tired, dep5_appetite,
               dep6_feelbad, dep7_concentrate, dep8_moveslow, dep9_dead, dep10_difficult)
                    )
                ) == 0
                )
                
    scoresphq <- data.frame(phq9_total, score_interpretation_phq9, phq9_treatment,data_complete_phq9)
    
	return(scoresphq)
    
}


#Calculate summary scores in data 
 phq9_scores <- adply(datphq9, 1, phq9)
 
 barplot(table(phq9_scores$score_interpretation_phq9), 
         col = c( "peachpuff", "mistyrose" ,"lavender", "lightblue", "lightskyblue3"), 
         main = "Count of subjects in each PHQ-9 Diagnosis category", 
         ylab = "Subject Count",
         names.arg = c("Minimal", "Mild", "Moderate", "Moderately Severe", "Severe" ))
 abline(col= "slategray", v=3.75, lwd= 2, lty = "dashed")
 mtext("                                                                                            Warrants treatment for depression", col= "slategray")

#Export data
write.csv( phq9_scores, "~/Biobank/23_PHQ9/phq9_reduced_data_export_20180124.csv",quote=T,row.names=F,na="#N/A")


