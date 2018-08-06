#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 18,LEC-5/ PCL-5(lifetime)
##########################################################################################

lecpcl<- function(dat0, exportdate)
{
#Only retain relevant variables
datlecpcl <- subset(dat0, select= c(assessment_id,vista_lastname,visit_number,
                              LEC_5_18_MostSevere
                        
              ))




#________________________________________________________________________________________              
# Completeness check
#----------------------------------------------------------------------------------------
lecpcl_score <- function(x)
{
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
#checking for completeness
  
  if(is.na(LEC_5_18_MostSevere)){
    completeness_lecpcl<-"not attempted"
  }else{completeness_lecpcl<-"complete"}


scores <- data.frame(completeness_lecpcl)

return(scores)
}


#Calculate summary scores in data 
datlecpcl_scored <- adply(datlecpcl, 1, lecpcl_score)


#to anonymize data
datlecpcl_scored1<- within(datlecpcl_scored,
                        {
                          assessment_id <- NULL
                          vista_lastname <- NULL
                        })

#________________________________________________________________________________________ 
#Export
#----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/18_LEC-5_PCL-5/LEC5_PCL5_scored_data_export.csv", sep="")
write.csv(datlecpcl_scored, filename,quote=T,row.names=F,na="#N/A")

filename <- paste("~/Biobank/18_LEC-5_PCL-5/LEC5_PCL5_scored_data_export_DEIDENTIFIED.csv", sep="")
write.csv(datlecpcl_scored1, filename,quote=T,row.names=F,na="#N/A")

print("18_LEC_done")

#return completness column
myvars <- c("assessment_id", "completeness_lecpcl")
newdata <- datlecpcl_scored[myvars]
return(newdata)
}