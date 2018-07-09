#########################################################################################
# Last Date modified: 06/27/2018
# Author: Katy Torres
# Description: Subset of question 29, Tinnitus Screener
##########################################################################################
tinnitus<- function(dat0, exportdate)
{
#Load plyr library
library(plyr)

#Only retain relevant variables
dattinnitus <- subset(dat0, 
              select= c(assessment_id,vista_lastname, visit_number,
                        Tinnitus.1_2.3mins,
                        Tinnitus.2_6months,
                        Tinnitus.3_quietroom,
                        Tinnitus.4_recentevents,
                        Tinnitus.5_comeandgo,
                        Tinnitus.6_experience
                        
              ))

#________________________________________________________________________________________              
# SCORING Functions Defined
#----------------------------------------------------------------------------------------
#Scoring function defined
tinnitus <- function(x)
{
  
  #attach(x)
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
  
#SCORING of categories
  
  no_tinnitus<- as.numeric(!Tinnitus.1_2.3mins)
  
  if(!is.na(Tinnitus.1_2.3mins) & !is.na(Tinnitus.5_comeandgo) &  !is.na(Tinnitus.4_recentevents)) {
    if(Tinnitus.4_recentevents ==1 & Tinnitus.5_comeandgo == 1 & Tinnitus.1_2.3mins==1) { tinnitus_temporary_only<- 1} 
    else{ tinnitus_temporary_only<- 0} 
  }
  else { tinnitus_temporary_only<- NA } 
  
  #CHECK THIS CODE
  if(!is.na(Tinnitus.6_experience) & !is.na(Tinnitus.1_2.3mins)) {
    if(Tinnitus.6_experience == 2 & Tinnitus.1_2.3mins == 1) { tinnitus_occassional<- 1}
    else{ tinnitus_occassional <- 0}
  } 
  else { tinnitus_occassional <- NA } 
  
  
  
  if(!is.na(Tinnitus.1_2.3mins) & !is.na(Tinnitus.2_6months)) {
    if(Tinnitus.2_6months==0 & Tinnitus.1_2.3mins==1) { tinnitus_acute<- 1} 
    else{ tinnitus_acute<- 0} 
  }
  else { tinnitus_acute <- NA } 
  
  
  
  if(!is.na(Tinnitus.1_2.3mins) & !is.na(Tinnitus.2_6months)) {
    if(Tinnitus.1_2.3mins == 1 & Tinnitus.2_6months == 1) { tinnitus_chronic<- 1} 
    else if(Tinnitus.1_2.3mins == 1 & Tinnitus.2_6months == 0) { tinnitus_chronic<- 0} 
    else if(Tinnitus.1_2.3mins == 0) { tinnitus_chronic <- 0} 
  }
  else { tinnitus_chronic <- NA } 

  
  #________________________________________________________________________________________              
  # Completeness check
  #----------------------------------------------------------------------------------------

  
  data_complete_tin <- as.numeric(
    sum(
      is.na(
        c(Tinnitus.1_2.3mins,
          Tinnitus.2_6months,
          Tinnitus.3_quietroom,
          Tinnitus.4_recentevents,
          Tinnitus.5_comeandgo,
          Tinnitus.6_experience
        )
      )
    ) == 0
  )
  
  data_not_attempted_tin <- as.numeric(
    sum(
      is.na(
        c(Tinnitus.1_2.3mins,
          Tinnitus.2_6months,
          Tinnitus.3_quietroom,
          Tinnitus.4_recentevents,
          Tinnitus.5_comeandgo,
          Tinnitus.6_experience
        )
      )
    ) == 6
)
  if(!(is.na(Tinnitus.1_2.3mins))){
    if(Tinnitus.1_2.3mins==0)
    {
      hasit<- 0}else{hasit <- 1}
  }else{hasit<-NA}
  
  
  completeness_tin<- "1"
  if(!(is.na(data_not_attempted_tin))){
    if(data_not_attempted_tin==1)
    {
      completeness_tin <- "not attempted"}else{}
  }else{}
  
  if(!(is.na(data_complete_tin))){
    if(data_complete_tin==1){
      completeness_tin <- "complete"} else{}
  }else{}

  if(!(is.na(hasit))){
    if(hasit==0){
      completeness_tin <- "complete"} else{}
  }else{}

  if(!(is.na(hasit))){
   if(data_not_attempted_tin==0 & data_complete_tin==0 & hasit==1){
     completeness_tin <- "partially completed"}else{}
  }else if (data_not_attempted_tin==0 & data_complete_tin==0){
    completeness_tin <- "partially completed"}
  else{}
  
  scores <- data.frame(no_tinnitus, tinnitus_temporary_only, tinnitus_occassional,
                       tinnitus_acute, tinnitus_chronic,
                       data_complete_tin, data_not_attempted_tin, completeness_tin)
  
  return(scores)
}


#Calculate summary scores in data
tinnitus_scores <- adply(dattinnitus, 1, tinnitus)

#________________________________________________________________________________________ 
#Export
#----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/29_Tinnitus_Screener/tinnitus_screener_reduced_data_export_", exportdate, ".csv", sep="")
write.csv(tinnitus_scores, filename,quote=T,row.names=F,na="#N/A")

print("29_Tinnitus_done")

#return completness column
myvars <- c("assessment_id", "completeness_tin")
newdata <- tinnitus_scores[myvars]
return(newdata)
}







