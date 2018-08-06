#########################################################################################
# Last Date modified: 06/21/2018
# Author: Katy Torres
# Description: Subset of question 24, Positive and Negative Affect Schedule
##########################################################################################
panas<- function(dat0, exportdate)
{
#Load plyr library
library(plyr)
  
#Only retain relevant variables
datpanas <- subset(dat0, select= c(assessment_id,vista_lastname,visit_number,
                        panas1_interest,
                        panas2_distress,
                        panas3_excite,
                        panas4_upset,
                        panas5_strong,
                        
                        panas6_guilt,
                        panas7_scare,
                        panas8_host,
                        panas9_enth,
                        panas10_proud,
                        
                        panas11_irri,
                        panas12_alert,
                        panas13_asham,
                        panas14_insp,
                        panas15_nerv,
                        
                        panas16_deter,
                        panas17_atten,
                        panas18_jitt,
                        panas19_act,
                        panas20_afraid,
                        
                        Panas.Positive,
                        Positive.Affect.Score,
                        Negative.Affect.Score,
                        PANAS.Positive,
                        Liz.s.Formula
                        
              ))
#________________________________________________________________________________________
# Data Manipulation and cleaning
#----------------------------------------------------------------------------------------

#________________________________________________________________________________________              
# SCORING Functions Defined
#----------------------------------------------------------------------------------------
#Scoring function defined
pan_score <- function(x)
{
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
Positive_Affect_Score <-panas1_interest +
                        panas3_excite +
                        panas5_strong +
                        panas9_enth +
                        panas10_proud +
                        panas12_alert +
                        panas14_insp +
                        panas16_deter +
                        panas17_atten +
                        panas19_act

Negative_Affect_Score <-panas2_distress +
                        panas4_upset +
                        panas6_guilt +
                        panas7_scare +
                        panas8_host +
                        panas11_irri +
                        panas13_asham +
                        panas15_nerv +
                        panas18_jitt +
                        panas20_afraid


data_complete_positive <- as.numeric( 
  sum(
    is.na(
      c(panas1_interest ,
        panas3_excite ,
        panas5_strong ,
        panas9_enth ,
        panas10_proud ,
        panas12_alert ,
        panas14_insp ,
        panas16_deter ,
        panas17_atten ,
        panas19_act)
    )
  ) == 0
)

data_complete_negative <- as.numeric( 
  sum(
    is.na(
      c(panas2_distress,
        panas4_upset,
        panas6_guilt,
        panas7_scare,
        panas8_host,
        panas11_irri,
        panas13_asham,
        panas15_nerv,
        panas18_jitt,
        panas20_afraid)
    )
  ) == 0
)

data_complete_panas <- as.numeric( 
  sum(
    is.na(
      c(panas2_distress,
        panas4_upset,
        panas6_guilt,
        panas7_scare,
        panas8_host,
        panas11_irri,
        panas13_asham,
        panas15_nerv,
        panas18_jitt,
        panas20_afraid,
        panas2_distress,
        panas4_upset,
        panas6_guilt,
        panas7_scare,
        panas8_host,
        panas11_irri,
        panas13_asham,
        panas15_nerv,
        panas18_jitt,
        panas20_afraid)
    )
  ) == 0
)

data_not_attempted_panas <- as.numeric( 
  sum(
    is.na(
      c(panas2_distress,
        panas4_upset,
        panas6_guilt,
        panas7_scare,
        panas8_host,
        panas11_irri,
        panas13_asham,
        panas15_nerv,
        panas18_jitt,
        panas20_afraid,
        panas2_distress,
        panas4_upset,
        panas6_guilt,
        panas7_scare,
        panas8_host,
        panas11_irri,
        panas13_asham,
        panas15_nerv,
        panas18_jitt,
        panas20_afraid)
    )
  ) == 20
)


completeness_panas<- "1"
if(!(is.na(data_not_attempted_panas))){
  if(data_not_attempted_panas==1)
  {
    completeness_panas <- "not attempted"}else{}
}else{completeness_panas<-NA}

if(!(is.na(data_complete_panas))){
  if(data_complete_panas==1){
    completeness_panas <- "complete"} else{}
}else{completeness_panas<-NA}


if(data_not_attempted_panas==0 & data_complete_panas==0 ){
  completeness_panas <- "partially completed"}else{}

scorespanas <- data.frame(Positive_Affect_Score, Negative_Affect_Score, data_complete_positive, data_complete_negative, data_complete_panas, data_not_attempted_panas, completeness_panas)

return(scorespanas)
}


#Calculate summary scores in data 
panas_scores <- adply(datpanas, 1, pan_score)

#to anonymize data
panas_scores1<- within(panas_scores,
                        {
                          assessment_id <- NULL
                          vista_lastname <- NULL
                        })


#________________________________________________________________________________________ 
#Export
#----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/24_PANAS/PANAS_scored_data_export.csv", sep="")
write.csv(panas_scores, filename,quote=T,row.names=F,na="#N/A")

filename <- paste("~/Biobank/24_PANAS/PANAS_scored_data_export_DEIDENTIFIED.csv", sep="")
write.csv(panas_scores1, filename,quote=T,row.names=F,na="#N/A")

print("24_PANAS_done")


#return completness column
myvars <- c("assessment_id", "completeness_panas")
newdata <- panas_scores[myvars]
return(newdata)
}


