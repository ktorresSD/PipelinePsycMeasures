#########################################################################################
# Last Date modified: 06/21/2018
# Author: Katy Torres
# Description: Subset of question 25, PROMIS PAIN INTENSITY
##########################################################################################
promis<- function(dat0, exportdate)
{
  
#Load plyr library
library(plyr)

#Only retain relevant variables
datpain <- subset(dat0, 
              select= c(assessment_id,vista_lastname,visit_number,
                        pain_level,
                        pain_intensity,
                        pain_average,
                        
                        pain_interfere_life,
                        pain_interfere_conc,
                        pain_interfere_day,
                        pain_interfere_rec,
                        pain_interfere_task,
                        
                        pain_interfere_social,
                        pain_score_intensity,
                        pain_score_interference
              ))
#________________________________________________________________________________________
# Data Manipulation and cleaning
#----------------------------------------------------------------------------------------

#________________________________________________________________________________________              
# SCORING Functions Defined
#----------------------------------------------------------------------------------------
pain_scoring <- function(x)
{
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
pain_Score<- pain_level + pain_intensity + pain_average
data_complete_promis <- as.numeric( 
  sum(
    is.na(
      c(pain_level,
        pain_intensity,
        pain_average)
    )
  ) == 0
)

data_not_attempted_promis <- as.numeric( 
  sum(
    is.na(
      c(pain_level,
        pain_intensity,
        pain_average)
    )
  ) == 3
)

if(!(is.na(data_not_attempted_promis))){
  if(data_not_attempted_promis==1)
  {
    completeness_promis <- "not attempted"}else{}
}else{completeness_promis<-NA}

if(!(is.na(data_complete_promis))){
  if(data_complete_promis==1){
    completeness_promis <- "complete"} else{}
}else{completeness_promis<-NA}

if(data_not_attempted_promis==0 & data_complete_promis==0){
  completeness_promis <- "partially completed"}else{}

#return back newly calculated scores

painscores <- data.frame(pain_Score,data_complete_promis, data_not_attempted_promis, completeness_promis)
return(painscores)
}


#Calculate summary scores in data
promis_scored <- adply(datpain, 1, pain_scoring)

#________________________________________________________________________________________ 
#Export
#----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/25_PROMIS_Pain_Intensity/PROMIS_Pain_Intensity_scored_data_export.csv", sep="")
write.csv( promis_scored, filename,quote=T,row.names=F,na="#N/A")

print("25_PROMIS_done")

#return completness column
myvars <- c("assessment_id", "completeness_promis")
newdata <- promis_scored[myvars]
return(newdata)
}



