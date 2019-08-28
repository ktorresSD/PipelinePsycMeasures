#########################################################################################
# Last Date modified: 06/27/2018
# Author: Katy Torres
# Description: Subset of question 22, PHQ15_22
##########################################################################################

phq15<- function(dat0, exportdate)
{
#Load plyr library
library(plyr)

#Only retain relevant variables
datphq15 <- subset(dat0, 
              select= c(assessment_id,vista_lastname,visit_number,
                        health1_stomach,
                        health2_back,
                        health3_arm,
                        health4_cramp,
                        health5_headache,
                        health6_chest,
                        health7_dizzy,
                        health8_faint,
                        health9_heart,
                        health10_breath,
                        health11_sex,
                        health12_constipation,
                        health13_nausea,
                        health14_tired,
                        health15_sleeping,
                        
                        health_score_phq15,
                        health_score_phq14
              ))

#________________________________________________________________________________________              
# SCORING Functions Defined
#----------------------------------------------------------------------------------------
score <- function(x)
{

  for (v in 1:length(x)) assign(names(x)[v], x[[v]])

	#PHQ-15 summary score is the summation of items 1-15
	#Note: This function is not designed to handle NA values (subject must have complete data)
  

  phq15_score_total <- health1_stomach +
                            health2_back+
                            health3_arm+
                            health4_cramp+
                            health5_headache+
                            health6_chest+
                            health7_dizzy+
                            health8_faint+
                            health9_heart+
                            health10_breath+
                            health11_sex+
                            health12_constipation+
                            health13_nausea+
                            health14_tired+
                            health15_sleeping
  
  
  phq14_score_males <-  health1_stomach +
                        health2_back +
                        health3_arm +
                        health5_headache +
                        health6_chest +
                        health7_dizzy +
                        health8_faint +
                        health9_heart +
                        health10_breath +
                        health11_sex +
                        health12_constipation +
                        health13_nausea +
                        health14_tired +
                        health15_sleeping

	phq15_minimal <- as.numeric(phq15_score_total <= 4)
	phq15_low <- as.numeric(phq15_score_total >= 5 & phq15_score_total <= 9)
	phq15_medium <- as.numeric(phq15_score_total >= 10 & phq15_score_total <= 14)
	phq15_high<- as.numeric(phq15_score_total >= 15)

	
	score_interpretation_PHQ15<- "0"
	if(!(is.na(phq15_minimal))){
	  if(phq15_minimal==1)
	  {
	    score_interpretation_PHQ15 <- "minimal"}else{}
	}else{score_interpretation_PHQ15<-NA}
	
	if(!(is.na(phq15_low))){
	  if(phq15_low== 1){
	    score_interpretation_PHQ15 <- "low"} else{}
	}else{score_interpretation_PHQ15<-NA}
	
	if(!(is.na(phq15_medium))){
	  if(phq15_medium==1){
	    score_interpretation_PHQ15 <- "medium"}else{}
	}else{score_interpretation_PHQ15<-NA}
	
	if(!(is.na(phq15_high))){
	  if(phq15_high==1){
	    score_interpretation_PHQ15 <- "high"}else{}
	}else{score_interpretation_PHQ15<-NA}

    data_complete_phq15 <- as.numeric(
                sum(
                    is.na(
                    c(health1_stomach,
                      health2_back,
                      health3_arm,
                      health5_headache,
                      health6_chest,
                      health7_dizzy,
                      health8_faint,
                      health9_heart,
                      health10_breath,
                      health11_sex,
                      health12_constipation,
                      health13_nausea,
                      health14_tired,
                      health15_sleeping
               )
                    )
                ) == 0
                )
    
    data_not_attempted_phq15 <- as.numeric(
      sum(
        is.na(
          c(health1_stomach,
            health2_back,
            health3_arm,
            health4_cramp,
            health5_headache,
            health6_chest,
            health7_dizzy,
            health8_faint,
            health9_heart,
            health10_breath,
            health11_sex,
            health12_constipation,
            health13_nausea,
            health14_tired,
            health15_sleeping
          )
        )
      ) == 15
    )
    
    completeness_phq15<- "1"
    if(!(is.na(data_not_attempted_phq15))){
      if(data_not_attempted_phq15==1)
      {
        completeness_phq15 <- "not attempted"}else{}
    }else{completeness_phq15<-NA}
    
    if(!(is.na(data_complete_phq15))){
      if(data_complete_phq15==1){
        completeness_phq15 <- "complete"} else{}
    }else{completeness_phq15<-NA}
    
    
    if(data_not_attempted_phq15==0 & data_complete_phq15==0){
      completeness_phq15 <- "partially completed"}else{}
    

    scoresphq15 <- data.frame(phq15_score_total, phq14_score_males ,phq15_minimal, phq15_low, phq15_medium, phq15_high, score_interpretation_PHQ15, data_complete_phq15, data_not_attempted_phq15, completeness_phq15)

	return(scoresphq15)
}


#Calculate summary scores in data
 score_datphq15 <- adply(datphq15 , 1, score)

 #to anonymize data
 score_datphq151<- within(score_datphq15,
                         {
                           assessment_id <- NULL
                           vista_lastname <- NULL
                         })
 
 
 
 
 #________________________________________________________________________________________ 
 #For Report
 #----------------------------------------------------------------------------------------

 library(psych)

 #subset by visit to get report information
 v1 <- score_datphq15[ which(score_datphq15$visit_number==1), ]
 v2 <- score_datphq15[ which(score_datphq15$visit_number==2), ]
 v3 <- score_datphq15[ which(score_datphq15$visit_number==3), ]
 
 #completeness table
 table(score_datphq15$completeness_phq15, score_datphq15$visit_number)
 
 #summary statistics for total PCL
 describe(v1$phq15_score_total)
 describe(v2$phq15_score_total)
 describe(v3$phq15_score_total)
 describe(score_datphq15$phq15_score_total)
 
 #mode
 Mode <- function(x) {
   ux <- unique(x)
   ux[which.max(tabulate(match(x, ux)))]
 }
 
 Mode(v1$phq15_score_total)
 Mode(v2$phq15_score_total)
 Mode(v3$phq15_score_total)
 Mode(score_datphq15$phq15_score_total)
 
 
 
 #histograms
 par(mfrow=c(2,2))
 hist(score_datphq15$phq15_score_total, breaks=10, xlab = "PHQ-15 Score", ylim=c(0,30), col = c("lightyellow"), main = "PHQ-15 total Score (all visits)")
 hist(v1$phq15_score_total, breaks=10, xlab = "PHQ-15 Score", ylim=c(0,30), col = c("lightyellow"), main = "PHQ-15 total Score (visit 1 only)")
 hist(v2$phq15_score_total, breaks=10, xlab = "PHQ-15 Score", ylim=c(0,30), col = c("lightyellow"), main = "PHQ-15 total Score (visit 2 only)")
 hist(v3$phq15_score_total, breaks=10, xlab = "PHQ-15 Score", ylim=c(0,30), col = c("lightyellow"), main = "PHQ-15 total Score (visit 3 only)")
 
 
 #reorder factors
 score_datphq15$score_interpretation_PHQ15 <- as.factor(score_datphq15$score_interpretation_PHQ15)
 score_datphq15$score_interpretation_PHQ15<- factor(score_datphq15$score_interpretation_PHQ15, levels = c("minimal", "low", "medium", "high"))
 
 par(mfrow=c(1,1))
 # Subject count for each PHQ-15 category
 barplot(table(score_datphq15$score_interpretation_PHQ15), 
         col = c( "peachpuff", "mistyrose" ,"lavender", "lightblue"), 
         main = "Assessments Count in each PHQ-15 severity categories", 
         ylab = "Assessment Count")


 

 #________________________________________________________________________________________ 
 #Export
 #----------------------------------------------------------------------------------------
 filename <- paste("~/Biobank/22_PHQ-15/PHQ15_22_scored_data_export.csv", sep="")
 write.csv(score_datphq15, filename,quote=T,row.names=F,na="NA")
 
 
 filename <- paste("~/Biobank/22_PHQ-15/PHQ15_22_scored_data_export_DEIDENTIFIED.csv", sep="")
 write.csv(score_datphq151, filename,quote=T,row.names=F,na="NA")
 
print("22_PHQ15_done")


#return completness column
myvars <- c("assessment_id", "completeness_phq15")
newdata <- score_datphq15[myvars]
return(newdata)
}



