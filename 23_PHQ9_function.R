#########################################################################################
# Last Date modified: 5/15/2018
# Author: Katy Torres
# Description: Subset of question 23, PHQ9
##########################################################################################
phq9<- function(dat0, exportdate)
{
#Load plyr library
 library(plyr)

#Only retain relevant variables
 datphq9 <- subset(dat0, 
               select= c(assessment_id,vista_lastname,visit_number,
               dep1_interest,
               dep2_down,
               dep3_sleep,
               dep4_tired,
               dep5_appetite,
               dep6_feelbad,
               dep7_concentrate,
               dep8_moveslow,
               dep9_dead,
               dep10_difficult))
              
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
    #flag if all entires are complete. 0 if incomplete data
    data_not_attempted_phq9 <- as.numeric( 
      sum(
        is.na(
          c(dep1_interest, dep2_down, dep3_sleep, dep4_tired, dep5_appetite,
            dep6_feelbad, dep7_concentrate, dep8_moveslow, dep9_dead, dep10_difficult)
        )
      ) == 10
    )
    
    #if the total score is 0, they have to have a 0 in question 8 
    
    if((is.na(phq9_total) | is.na(dep10_difficult))){inconsistency_flag <- NA
    }else if(phq9_total==0 & dep10_difficult == 0){ inconsistency_flag <- "consistent"
    } else if (phq9_total ==0 & dep10_difficult < 0) {inconsistency_flag <- "inconsistent"
    } else if (phq9_total > 0 ){inconsistency_flag <- "total not zero"}
    
    
    
    completeness_phq9<- "1"
    if(!(is.na(data_not_attempted_phq9))){
      if(data_not_attempted_phq9==1)
      {
        completeness_phq9 <- "not attempted"}else{}
    }else{completeness_phq9<-NA}
    
    if(!(is.na(data_complete_phq9))){
      if(data_complete_phq9==1){
        completeness_phq9 <- "complete"} else{}
    }else{completeness_phq9<-NA}
    
    if(data_not_attempted_phq9==0 & data_complete_phq9==0 ){
      completeness_phq9 <- "partially completed"}else{}
    
                
    scoresphq <- data.frame(phq9_total, score_interpretation_phq9, inconsistency_flag,  phq9_treatment,data_complete_phq9, data_not_attempted_phq9, completeness_phq9)
    
	return(scoresphq)
    
}


#Calculate summary scores in data 
 phq9_scores <- adply(datphq9, 1, phq9)
 

#check for consistency between PHQ9 total scores and 10th question
plot(phq9_scores$phq9_total,phq9_scores$dep10_difficult, xlab= "PHQ-9 Total Scores", ylab= "Impact of problems", main= "Total scores and impact of problems on quality of life \n (Checking for inconsistency). ", pch= 16, col= "deepskyblue3")

 
 #________________________________________________________________________________________              
 # Descriptive Stats and plots
 #----------------------------------------------------------------------------------------
 
 #subset by visit to get report information
 v1 <- phq9_scores[ which(phq9_scores$visit_number==1), ]
 v2 <- phq9_scores[ which(phq9_scores$visit_number==2), ]
 v3 <- phq9_scores[ which(phq9_scores$visit_number==3), ]
 
 #completeness table
 table(phq9_scores$completeness_phq9, phq9_scores$visit_number)
 
 #summary statistics for total PCL
 describe(v1$phq9_total)
 describe(v2$phq9_total)
 describe(v3$phq9_total)
 describe(phq9_scores$phq9_total)
 
 #mode
 Mode <- function(x) {
   ux <- unique(x)
   ux[which.max(tabulate(match(x, ux)))]
 }
 
 Mode(v1$phq9_total)
 Mode(v2$phq9_total)
 Mode(v3$phq9_total)
 Mode(phq9_scores$phq9_total)
 
 
 #histograms
 par(mfrow=c(2,2))
 hist(phq9_scores$phq9_total, breaks=10, xlab = "PHQ9 Score", ylim=c(0,60), col = c("lightyellow"), main = "PHQ9 total Score (all visits)")
 hist(v1$phq9_total, breaks=10, xlab = "PHQ9 Score", ylim=c(0,60), col = c("lightyellow"), main = "PHQ9 total Score (visit 1 only)")
 hist(v2$phq9_total, breaks=10, xlab = "PHQ9 Score", ylim=c(0,60), col = c("lightyellow"), main = "PHQ9 total Score (visit 2 only)")
 hist(v3$phq9_total, breaks=10, xlab = "PHQ9 Score", ylim=c(0,60), col = c("lightyellow"), main = "PHQ9 total Score (visit 3 only)")
 
 
 par(mfrow=c(1,1))
 hist(phq9_scores$phq9_total, breaks=10, xlim=c(0,27), xlab = "Total PHQ9 Score", col = c("steelblue3"), main = "Histogram for Total PHQ9 Score")
 abline(v = 14, lty = 2, lwd=2)
 axis(1, 1:27)
 legend('topright',legend=c("Cut-off"),lty = 2, lwd=2)

 
 barplot(table(phq9_scores$score_interpretation_phq9), 
         col = c( "peachpuff", "mistyrose" ,"lavender", "lightblue", "lightskyblue3"), 
         main = "Count of subjects in each PHQ-9 Diagnosis category", 
         ylab = "Subject Count",
         names.arg = c("Minimal", "Mild", "Moderate", "Moderately Severe", "Severe" ))
 abline(col= "slategray", v=3.70, lwd= 2, lty = "dashed")
 mtext("                                                                                            Warrants treatment for depression", col= "slategray")
 
 
 
 
 
 #to anonymize data
 #----------------------------------------------------------------------------------------
 phq9_scores1<- within(phq9_scores,
                       {
                         assessment_id <- NULL
                         vista_lastname <- NULL
                       })
 
 
 
#________________________________________________________________________________________ 
#Export
#----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/23_PHQ9/phq9_scored_data_export.csv", sep="")
write.csv(phq9_scores, filename,quote=T,row.names=F,na="NA")

filename <- paste("~/Biobank/23_PHQ9/phq9_scored_data_export_DEIDENTIFIED.csv", sep="")
write.csv(phq9_scores1, filename,quote=T,row.names=F,na="NA")


print("23_PHQ9_done")

#return completness column
myvars <- c("assessment_id", "completeness_phq9")
newdata <- phq9_scores[myvars]
return(newdata)
}


