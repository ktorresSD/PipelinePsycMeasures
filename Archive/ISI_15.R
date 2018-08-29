#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 15, ISI
##########################################################################################

#Load plyr library
library(plyr)

#To the user: Set path to where data is stored
setwd("~/Biobank/data")
#________________________________________________________________________________________
#READ AND SUBSET LARGE DATA TO ONLY CONTAIN DESIRED QUESTIONAIRE VARIABLES
#----------------------------------------------------------------------------------------
#Read all data
dat0 <- read.csv('joined_data_export_20171221.csv',header=T,na.strings=c(NA,999))

#Only retain relevant variables
datisi <- subset(dat0, 
              select= c(assessment_id,vista_lastname,
                        sleep1a_falling,
                        sleep1b_staying,
                        sleep1c_waking,
                        sleep2_satisfied,
                        sleep3_interfere,
                        sleep4_noticeable,
                        sleep5_worried,
                        sleep_score
              ))
#________________________________________________________________________________________
# Data Manipulation and cleaning
#----------------------------------------------------------------------------------------

#________________________________________________________________________________________              
# SCORING Functions Defined
#----------------------------------------------------------------------------------------
#Scoring function defined
insomnia <- function(x)
{
  
  #attach(x)
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
  
  #summary score is the summation of items 1-7
  #Note: This function is not designed to handle NA values (subject must have complete data)
  
  
  insomnia_total <-   sleep1a_falling+
  sleep1b_staying+
  sleep1c_waking+
  sleep2_satisfied+
  sleep3_interfere+
  sleep4_noticeable+
  sleep5_worried+
  sleep_score
  
  
  not_clinically_significant_insomnia <- as.numeric(insomnia_total <= 7)
  subthreshold_insomnia <- as.numeric(insomnia_total >= 8 & insomnia_total <= 14)
  moderate_severity_insomnia <- as.numeric(insomnia_total >= 15 & insomnia_total <= 21)
  severe_insomnia <- as.numeric(insomnia_total >= 22)
  
  
  insomnia_incomplete <- sum(c(sleep1a_falling,
                             sleep1b_staying,
                             sleep1c_waking,
                             sleep2_satisfied,
                             sleep3_interfere,
                             sleep4_noticeable,
                             sleep5_worried,
                             sleep_score),na.rm=T)

  insomnia_data_complete <- as.numeric(
    sum(
      is.na(
        c(sleep1a_falling,
          sleep1b_staying,
          sleep1c_waking,
          sleep2_satisfied,
          sleep3_interfere,
          sleep4_noticeable,
          sleep5_worried,
          sleep_score
        )
      )
    ) == 0
  )
  
  scoresisi <- data.frame(insomnia_total, not_clinically_significant_insomnia,  subthreshold_insomnia, moderate_severity_insomnia, severe_insomnia , insomnia_incomplete, insomnia_data_complete)
  
  return(scoresisi)
}


#Calculate summary scores in data
insomnia_scores <- adply(datisi, 1, insomnia)

#________________________________________________________________________________________ 
#Export data
#----------------------------------------------------------------------------------------
write.csv( insomnia_scores, "~/Biobank/15_ISI/ISI_reduced_data_export_20180515.csv",quote=T,row.names=F,na="#N/A")


