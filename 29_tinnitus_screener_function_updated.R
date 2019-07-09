#########################################################################################
# Last Date modified: 06/19/2019
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
                        Tinnitus1_2to3mins,
                        Tinnitus2_6_months,
                        Tinnitus3_quiteroom,
                        Tinnitus4_recentevents,
                        Tinnitus5_comeandgo,
                        Tinnitus6_experience))

#________________________________________________________________________________________              
# SCORING Functions Defined
#----------------------------------------------------------------------------------------
tinnitus <- function(x)
{
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
  
#SCORING of categories
  no_tinnitus<- as.numeric(!Tinnitus1_2to3mins)
  
  if(!is.na(Tinnitus1_2to3mins) & !is.na(Tinnitus2_6_months)) {
    if(Tinnitus1_2to3mins==1 & Tinnitus2_6_months==0) { tinnitus_acute<- 1 
    }else{ tinnitus_acute<- 0} 
  }
  else { tinnitus_acute <- NA } 
  
  if(!is.na(Tinnitus1_2to3mins) & !is.na(Tinnitus2_6_months)) {
    if(Tinnitus1_2to3mins == 1 & Tinnitus2_6_months == 1) { tinnitus_chronic<- 1
    }else {tinnitus_chronic<- 0}
  } else {tinnitus_chronic <- NA } 
  
  if(!is.na(Tinnitus1_2to3mins) & !is.na(Tinnitus3_quiteroom)) {
    if(Tinnitus1_2to3mins==1 & Tinnitus3_quiteroom==3) { tinnitus_constant<- 1} 
    else if(Tinnitus1_2to3mins==1 & Tinnitus3_quiteroom==2) { tinnitus_constant<- 1} 
    else{ tinnitus_constant<- 0} 
  }
  else { tinnitus_constant <- NA } 
  
  if(!is.na(Tinnitus1_2to3mins) &  !is.na(Tinnitus3_quiteroom) &!is.na(Tinnitus5_comeandgo) &  !is.na(Tinnitus4_recentevents)) {
    if(Tinnitus1_2to3mins==1 & Tinnitus3_quiteroom == 1 & Tinnitus4_recentevents ==2) { tinnitus_temporary_only<- 1}
    else if(Tinnitus1_2to3mins==1 & Tinnitus3_quiteroom == 1 & Tinnitus5_comeandgo == 0  ) { tinnitus_temporary_only<- 1
    }else{ tinnitus_temporary_only<- 0} 
  }else { tinnitus_temporary_only<- NA } 

  
  if(!is.na(Tinnitus6_experience) & !is.na(Tinnitus1_2to3mins) &  !is.na(Tinnitus3_quiteroom) ) {
    if(Tinnitus1_2to3mins == 1 & Tinnitus3_quiteroom==1 & Tinnitus6_experience == 1 ) { tinnitus_intermittent<- 1}
    else{tinnitus_intermittent <- 0}
  } 
  else {tinnitus_intermittent <- NA } 
  
  if(!is.na(Tinnitus6_experience) & !is.na(Tinnitus1_2to3mins)&  !is.na(Tinnitus3_quiteroom)) {
    if(Tinnitus1_2to3mins == 1 & Tinnitus3_quiteroom==1 & Tinnitus6_experience == 2) { tinnitus_occassional<- 1}
    else{ tinnitus_occassional <- 0}
  } 
  else { tinnitus_occassional <- NA } 
  
  #________________________________________________________________________________________              
  # Completeness check
  #----------------------------------------------------------------------------------------
  data_complete_tin <- as.numeric(
    sum(
      is.na(
        c(Tinnitus1_2to3mins,
          Tinnitus2_6_months,
          Tinnitus3_quiteroom,
          Tinnitus4_recentevents,
          Tinnitus5_comeandgo,
          Tinnitus6_experience
        )
      )
    ) == 0
  )
  
  data_not_attempted_tin <- as.numeric(
    sum(
      is.na(
        c(Tinnitus1_2to3mins,
          Tinnitus2_6_months,
          Tinnitus3_quiteroom,
          Tinnitus4_recentevents,
          Tinnitus5_comeandgo,
          Tinnitus6_experience
        )
      )
    ) == 6
)
  if(!(is.na(Tinnitus1_2to3mins))){
    if(Tinnitus1_2to3mins==0)
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
                       tinnitus_acute, tinnitus_chronic, tinnitus_intermittent, tinnitus_constant,
                       data_complete_tin, data_not_attempted_tin, completeness_tin)
  
  return(scores)
}


#Calculate summary scores in data
tinnitus_scores <- adply(dattinnitus, 1, tinnitus)

#to anonymize data
tinnitus_scores1<- within(tinnitus_scores,
                        {
                          assessment_id <- NULL
                          vista_lastname <- NULL
                        })


#________________________________________________________________________________________              
# Descriptive Stats and plots
#----------------------------------------------------------------------------------------


#completeness table
table(tinnitus_scores$completeness_tin, tinnitus_scores$visit_number)

table(tinnitus_scores$no_tinnitus)
table(tinnitus_scores$tinnitus_temporary_only)
table(tinnitus_scores$tinnitus_occassional)
table(tinnitus_scores$tinnitus_acute)
table(tinnitus_scores$tinnitus_chronic)
table(tinnitus_scores$tinnitus_intermittent)
table(tinnitus_scores$tinnitus_constant)


#________________________________________________________________________________________ 
#Export
#----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/29_Tinnitus_Screener/tinnitus_screener_scored_data_export1.csv", sep="")
write.csv(tinnitus_scores, filename,quote=T,row.names=F,na="NA")

filename <- paste("~/Biobank/29_Tinnitus_Screener/tinnitus_screener_scored_data_export_DEIDENTIFIED.csv", sep="")
write.csv(tinnitus_scores1, filename,quote=T,row.names=F,na="NA")


print("29_Tinnitus_done")

#return completness column
myvars <- c("assessment_id", "completeness_tin")
newdata <- tinnitus_scores[myvars]
return(newdata)
}







