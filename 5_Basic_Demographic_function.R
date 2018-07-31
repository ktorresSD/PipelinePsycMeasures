#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 5, Basic Demographic and scoring functions
##########################################################################################
basicdemo<- function(dat0, exportdate)
{

#Only retain relevant variables
datdemo <- subset(dat0, 
              select= c(assessment_id,vista_lastname,visit_number,
                        date_created,
                        demo_gender_r,
                        demo_YOB_r,
                        demo_weight_r,
                        demo_heightft_r,
                        demo_heightinch_r,
                        demo_ethnic_r,
                        
                        demo_racewhite,
                        demo_race_black,
                        demo_race_amind,
                        demo_race_pacisl,
                        demo_race_asian,
                        demo_race_decline,
                        demo_race_oth,
                        
                        demo_relationship_r
                        
              ))
datdemo$year_assessed<- format(as.Date(datdemo$date_created, format="%d/%m/%Y"),"%Y")
datdemo$approx_age<- as.numeric(datdemo$year_assessed) - datdemo$demo_YOB_r


#________________________________________________________________________________________ 
#Export datBTBISa
#----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/5_Basic_Demographic/Basic_Demographic_scored_data_export.csv", sep="")
write.csv(datdemo, filename,quote=T,row.names=F,na="#N/A")

return(print("5_Basic_Demographic_done"))
}

