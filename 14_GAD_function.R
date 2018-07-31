#########################################################################################
# Last Date modified: 07/03/2018
# Author: Katy Torres
# Description: Subset of question 14, GAD7 scoring functions
##########################################################################################

gad<- function(dat0, exportdate)
{
  #Load plyr library
  library(plyr)
  library(psych)
#Only retain relevant variables
 datgad <- subset(dat0, 
               select= c(assessment_id,vista_lastname,visit_number,
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
         
  #Score Interpretation
   mild_anxiety <- as.numeric(gad7_total >= 5 & gad7_total <= 9)
   moderate_anxiety <- as.numeric(gad7_total >= 10 & gad7_total <= 14)
   severe_anxiety <- as.numeric(gad7_total >= 15)
    
    
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
    
    
    #treatment action if total score is greater than 14
    
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
    
    #Completeness chack
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
    
    data_not_attempted_gad <- as.numeric( 
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
      ) == 7
    )
    
    completeness_gad<- "1"
    if(!(is.na(data_not_attempted_gad))){
      if(data_not_attempted_gad==1)
      {
        completeness_gad <- "not attempted"}else{}
    }else{completeness_gad<-NA}
    
    if(!(is.na(data_complete_gad))){
      if(data_complete_gad==1){
        completeness_gad <- "complete"} else{}
    }else{completeness_gad<-NA}
    
    if(data_not_attempted_gad==0 & data_complete_gad==0 ){
      completeness_gad <- "partially completed"}else{}
    
    gadscores <- data.frame(gad7_total, gad7_greater_than_cut_off, mild_anxiety, moderate_anxiety,  severe_anxiety, score_interpretation_gad7, data_complete_gad, data_not_attempted_gad, completeness_gad)
    
	return(gadscores)
}


#Calculate summary scores in data 
 gad7_scores <- adply(datgad, 1, gad7)
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
 
 # #saving this to a image file into folder
 plotname1 <- paste("~/Biobank/14_GAD7/gad7_histogram_of_total_score", exportdate, ".png", sep="")
 png(filename=plotname1)
 hist(gad7_scores$gad7_total, xlab = "Total GAD Score", col = c("steelblue3"), main = "Histogram for Total GAD Score")
 abline(v = 10, lty = 2, lwd=3)
 axis(1, 1:21)
 dev.off()
 
 barplot(table(gad7_scores$score_interpretation_gad7), 
         col = c( "peachpuff", "mistyrose" ,"lavender", "lightblue"), 
         main = "Count of subjects in each GAD Diagnosis category", 
         ylab = "Subject Count",
         names.arg = c("None", "Mild", "Moderate", "Severe"))
 abline(col= "slategray", v=2.5, lwd= 2, lty = "dashed")
 mtext("                                                  Possible diagnosis of GAD", col= "slategray")
 
 # #saving this to a image file into folder
  plotname <- paste("~/Biobank/14_GAD7/gad7_plot_", exportdate, ".png", sep="")
  png(filename=plotname)
  barplot(table(gad7_scores$score_interpretation_gad7), 
          col = c( "peachpuff", "mistyrose" ,"lavender", "lightblue"), 
          main = "Count of subjects in each GAD Diagnosis category", 
          ylab = "Subject Count",
          names.arg = c("None", "Mild", "Moderate", "Severe"))
  abline(col= "slategray", v=2.5, lwd= 2, lty = "dashed")
  mtext("                                                  Possible diagnosis of GAD", col= "slategray")
  
  dev.off()

 #________________________________________________________________________________________ 
 #Export
 #----------------------------------------------------------------------------------------
 filename <- paste("~/Biobank/14_GAD7/gad7_scored_data_export.csv", sep="")
 write.csv( gad7_scores, filename,quote=T,row.names=F,na="#N/A")
 
print("14_GAD_done")
 
 
 #return completness column
 myvars <- c("assessment_id", "completeness_gad")
 newdata <- gad7_scores[myvars]
 return(newdata)
}


