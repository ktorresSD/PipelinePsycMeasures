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

#________________________________________________________________________________________              
# SCORING Functions Defined
#----------------------------------------------------------------------------------------
painn_score <- function(x)
{
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
  
  #checking for completeness
  data_painn<-as.numeric(sum(is.na(c(pain_area_text_0, pain_number, pain_area_count))) == 0 )
  
  data_not_attempted_painn<- as.numeric(sum(is.na(c(pain_area_text_0, pain_number, pain_area_count))) == 3)
  
  
  completeness_painn<- "1"
  
  if(data_not_attempted_painn==1)
  {
    completeness_painn <- "not_attempted"}else{}
  
  if(!(is.na(data_painn))){
    if(data_painn==1){
      completeness_painn <- "complete"} else{}
  }else{}
  
  if(data_not_attempted_painn== 0 & data_painn==0){
    completeness_painn <- "partially completed"}else{}
  
  
  scores <- data.frame(data_not_attempted_painn, completeness_painn )
  
  return(scores)
}


#Calculate summary scores in data
datpain1 <- adply(datpain, 1, painn_score)



#to anonymize data
datpain2<- within(datpain1,
                  {
                    assessment_id <- NULL
                    vista_lastname <- NULL
                  })

#________________________________________________________________________________________ 
#Export datBTBISa
#----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/6_basic_pain/basic_pain_scored_data_export.csv", sep="")
write.csv(datpain1, filename,quote=T,row.names=F,na="NA")

filename <- paste("~/Biobank/6_basic_pain/basic_pain_scored_data_export_DEIDENTIFIED.csv", sep="")
write.csv(datpain2, filename,quote=T,row.names=F,na="NA")

return(print("6_basic_Pain_done"))
}


