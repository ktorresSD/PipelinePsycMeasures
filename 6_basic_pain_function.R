#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 6,basic_pain
##########################################################################################
basicpain <- function(dat0, exportdate)
{
#Load plyr library
library(plyr)

#Only retain relevant variables
datpain <- subset(dat0, select= c(assessment_id,vista_lastname,visit_number,
                              pain_area_text,
                              pain_area_text_0,
                              pain_area_text_1,
                              pain_area_text_2,
                              pain_area_text_3,
                              pain_area_text_4,
                              pain_area,
                              pain_number,
                              pain_area_count,
                              pain_formula
                              
              ))
#________________________________________________________________________________________              
# SCORING Functions Defined
#----------------------------------------------------------------------------------------
datpain$pain_score<-ifelse(datpain$pain_number >=4, TRUE, FALSE)

#to anonymize data
datpain1<- within(datpain,
                  {
                    assessment_id <- NULL
                    vista_lastname <- NULL
                  })

#________________________________________________________________________________________ 
#Export datBTBISa
#----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/6_basic_pain/basic_pain_scored_data_export.csv", sep="")
write.csv(datpain, filename,quote=T,row.names=F,na="#N/A")

filename <- paste("~/Biobank/6_basic_pain/basic_pain_scored_data_export_DEIDENTIFIED.csv", sep="")
write.csv(datpain1, filename,quote=T,row.names=F,na="#N/A")

return(print("6_basic_Pain_done"))
}


