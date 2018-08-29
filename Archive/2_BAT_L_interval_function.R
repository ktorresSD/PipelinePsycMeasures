#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 2, BAT-L interval (head injury)
##########################################################################################

batl2 <- function(dat0, exportdate)
{

#Only retain relevant variables
datbatli <- subset(dat0, 
              select= c(assessment_id,vista_lastname,visit_number,
                        BATL_Int_headinj,BATL_Int_howinj,BATL_timeuncon
              ))

#________________________________________________________________________________________ 
#Export data
#----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/2_BAT-L_interval/BAT-L_interval_reduced_data_export_", exportdate, ".csv", sep="")
write.csv(datbatli, filename,quote=T, row.names=F,na="#N/A")


return(print("2_BATL_done"))
}