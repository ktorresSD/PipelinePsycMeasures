#########################################################################################
# Last Date modified: 2/01/2018
# Author: Katy Torres
# Description: Subset of question 14, GAD7 scoring functions
##########################################################################################

#Load plyr library
 library(plyr)
library(psych)

#To the user: Set path to where data is stored
 setwd('C:/Users/Psychiatry Lab/Documents/Biobank/data')


#Read all data
 dat0 <- read.csv('joined_data_export_20180606.csv',header=T,na.strings=c(NA,999))

#Only retain relevant variables
 datgad <- subset(dat0, 
               select= c(assessment_id,vista_lastname,
               gad1_nervous,
               gad2_notable,
               gad3_worry,
               gad4_trouble,
               gad5_restless,
               gad6_annoyed,
               gad7_afraid,
               gad8_difficult,
               gad7_score))
              
#Scoring function defined
gad7 <- function(x){

    #attach(x)
    for (v in 1:length(x)) assign(names(x)[v], x[[v]])
    
	#GAD-7 summary score is the summation of items 1-7 (not 8)
	#Note: This function is not designed to handle NA values (subject must have complete data)

                           
	gad7_total <- gad1_nervous +
                gad2_notable +
                gad3_worry +
                gad4_trouble +
                gad5_restless +
                gad6_annoyed +
                gad7_afraid
         
  
   mild_anxiety <- as.numeric(gad7_total >= 5 & gad7_total <= 9)
   
   moderate_anxiety <- as.numeric(gad7_total >= 10 & gad7_total <= 14)
   
   severe_anxiety <- as.numeric(gad7_total >= 15)
   

    
   gad7_incomplete <- sum(c(gad1_nervous ,
	          gad2_notable ,
            gad3_worry ,
            gad4_trouble ,
            gad5_restless ,              
            gad6_annoyed ,
           gad7_afraid),na.rm=T)
                
    if(is.na(gad7_incomplete) )
    {
     gad7_poss_dx <- NA
    } else if(gad7_incomplete >= 10)
    {
     gad7_poss_dx <- 1
    } else if (gad7_incomplete< 10)
    {
     gad7_poss_dx <- 0
    } 
    
    data_complete_gad <- as.numeric( 
                sum(
                    is.na(
                    c(gad1_nervous,
               gad2_notable,
               gad3_worry,
               gad4_trouble,
               gad5_restless,
               gad6_annoyed,
               gad7_afraid
               )
                    )
                ) == 0
                )
    
    score_interpretation_gad7<- "1"
    if(!(is.na(mild_anxiety))){
      if(mild_anxiety==1)
      {
        score_interpretation_gad7 <- "2"}else{}
    }else{score_interpretation_gad7<-NA}

    if(!(is.na(moderate_anxiety))){
      if(moderate_anxiety==1){
        score_interpretation_gad7 <- "3"} else{}
    }else{score_interpretation_gad7<-NA}

    if(!(is.na(severe_anxiety))){
      if(severe_anxiety==1){
        score_interpretation_gad7 <- "4"}else{}
    }else{score_interpretation_gad7<-NA}
    
    # if((severe_anxiety==0) & (moderate_anxiety==0) & (mild_anxiety==0)){
    #   score_interpretation<-0
    # }else{}
    
    
    #treatment action if total score is greater than 10
    
    if (is.na(gad7_total))
    {
      gad7_greater_than_cut_off <- NA
    } else if (gad7_total < 10)
    {
      gad7_greater_than_cut_off <- 0
    } else if (gad7_total >= 10)
    {
      gad7_greater_than_cut_off <- 1
    }
    
    gadscores <- data.frame(gad7_total, gad7_greater_than_cut_off, mild_anxiety, moderate_anxiety,  severe_anxiety, score_interpretation_gad7, gad7_incomplete,gad7_poss_dx,data_complete_gad)
    
	return(gadscores)
}

#________________________________________________________________________________________              
# Descriptive Stats and plots
#----------------------------------------------------------------------------------------
#Calculate summary scores in data 
 gad7_scores <- adply(datgad, 1, gad7)
 
 #Descriptive stats
 describe(gad7_scores$gad7_total)
 #mode
 names(sort(-table(gad7_scores$gad7_total)))[1]
 
 table(gad7_scores$score_interpretation_gad7)
 table(gad7_scores$gad7_total)

#histogram of total score
 hist(gad7_scores$gad7_total, xlab = "Total GAD Score", col = c("steelblue3"), main = "Histogram for Total GAD Score")
 abline(v = 10, lty = 2, lwd=3)
 axis(1, 1:21)

 #________________________________________________________________________________________ 
 #Export data
 #----------------------------------------------------------------------------------------
 write.csv( gad7_scores, "~/Biobank/14_GAD7/gad7_reduced_data_export_20180606.csv",quote=T,row.names=F,na="#N/A")


