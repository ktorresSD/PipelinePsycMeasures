#########################################################################################
# Last Date modified: 06/21/2018
# Author: Katy Torres
# Description: Subset of question 26, Research ID and scoring functions
##########################################################################################

rid<- function(dat0, exportdate)
{
  
#Only retain relevant variables
datid <- subset(dat0, 
              select= c(assessment_id,vista_lastname, visit_number,
                        
                        Guilt_Visit,
                        Guilt_Comments,
                        
                        Research_VisitDate
              ))


#________________________________________________________________________________________ 
#Export
#----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/26_Research_ID/Research_ID_scored_data_export.csv", sep="")
write.csv( datid, filename,quote=T,row.names=F,na="#N/A")

return(print("26_Research_ID_done"))
}
