#########################################################################################
# Last Date modified: 06/21/2018
# Author: Katy Torres
# Description: Subset of question 19, MST 2016
##########################################################################################
mst<- function(dat0, exportdate)
{

#Only retain relevant variables
datmst <- subset(dat0, 
              select= c(assessment_id,vista_lastname, visit_number,
                        MST_2016_Q1_2,
                        #MST_2016_consult, will not output this one
                        MST2016_Research_1
                        
              ))

#function to merge old and new MST responses into new column to be used in future analysis

datmst$MST_v1 <-datmst$MST_2016_Q1_2
datmst$MST_v2 <- datmst$MST2016_Research_1

library(tidyr)

datmst[is.na(datmst)] = ''
mergedversion <- unite(datmst, MST_use, MST_2016_Q1_2:MST2016_Research_1, sep='')
mergedversion[mergedversion == ''] <- NA

#Subset by visit one only as this measure is only administered in the 1st visit
mergedversion1 <-mergedversion[mergedversion$visit_number==1,]

#________________________________________________________________________________________              
# Completeness Functions Defined
#----------------------------------------------------------------------------------------
mst_score <- function(x)
{
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
  

  
  #________________________________________________________________________________________              
  # Completeness check
  #----------------------------------------------------------------------------------------
  
  #checking for completeness
  data_complete_mst<- as.numeric(
    sum(
      is.na(
        c(MST_use
        )
      )
    ) == 0
  )
  
  data_not_attempted_mst<- as.numeric(
    sum(
      is.na(
        c(MST_use
        )
      )
    ) == 1
  )
  
  completeness_mst<- "-9"
  if(!(is.na(data_not_attempted_mst))){
    if(data_not_attempted_mst==1)
    {
      completeness_mst <- "not attempted"}else{}
  }else{}
  
  if(!(is.na(data_complete_mst))){
    if(data_complete_mst==1){
      completeness_mst <- "complete"} else{}
  }else{}

  scores <- data.frame( completeness_mst)
  
  return(scores)
}


#Calculate summary scores in data 
datmst_scored <- adply(mergedversion1, 1, mst_score)

datmst_scored1<- within(datmst_scored,
                        {
                          assessment_id <- NULL
                          vista_lastname <- NULL
                        })
# 
# #________________________________________________________________________________________ 
# #Report
# #----------------------------------------------------------------------------------------
# 
# #completeness table
# table(datmst_scored$completeness_mst)
# 
# #yes or no answer
# table(as.numeric(datmst_scored$MST_use))
# 
# 
# #plot
# counts <- table(datmst_scored$MST_use)
# barplot(counts, main="MST2016 for Research reponses", col = c("blue","red","gray"),
#         xlab="Reponse", ylab= "Number of Subjects", names.arg=c("No to both", "Yes to at least one", "Decline to answer")) 
#   
#________________________________________________________________________________________ 
#Export
#----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/19_MST2016/MST2016_scored_data_export.csv", sep="")
write.csv(datmst_scored , filename,quote=T,row.names=F,na="NA")

filename <- paste("~/Biobank/19_MST2016/MST2016_scored_data_export_DEIDENTIFIED.csv", sep="")
write.csv(datmst_scored1, filename,quote=T,row.names=F,na="NA")

print("19_mst_done")

#return completness column
myvars <- c("assessment_id", "completeness_mst")
newdata <- datmst_scored[myvars]
return(newdata)
}