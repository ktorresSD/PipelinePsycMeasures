#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 3, BAT-L 
##########################################################################################

#Load plyr library
 library(plyr)

#To the user: Set path to where data is stored
 setwd("~/Biobank/data")


#Read all data
 dat_0 <- read.csv('joined_data_export_20171221.csv',header=T,na.strings=c(NA,999))

#Only retain relevant variables
 dat_BATL <- subset(dat0, 
               select= c(assessment_id,vista_lastname,
               BATL1_crash,
               BATL2_motor,
               BATL3_terr,
               BATL4_ped,
               BATL5_obj,
               BATL6_equip,
               BATL7_stairs,
               BATL8_high,
               BATL9_faint,
               BATL10_drug,
               BATL11_bike,
               BATL12_roll,
               BATL13_horse,
               BATL14_ski,
               BATL15_sky,
               BATL16_sport,
               BATL17_play,
               BATL18_water,
               BATL19_abuse,
               BATL20_mugg,
               BATL21_mil,
               BATL22_comb,
               BATL23_other,
               BATL25_time,
               BATL24_worst))
               
               
               
              
#Scoring function defined
batl <- function(x)
{

    #attach(x)
    for (v in 1:length(x)) assign(names(x)[v], x[[v]])
    
	#BATL summary score is the summation of items 1-23
         
                
	batl_total <- BATL1_crash +
               BATL2_motor +
               BATL3_terr +
               BATL4_ped +
               BATL5_obj +
               BATL6_equip +
               BATL7_stairs +
               BATL8_high +
               BATL9_faint +
               BATL10_drug +
               BATL11_bike +
               BATL12_roll +
               BATL13_horse +
               BATL14_ski +
               BATL15_sky +
               BATL16_sport +
               BATL17_play +
               BATL18_water +
               BATL19_abuse +
               BATL20_mugg +
               BATL21_mil +
               BATL22_comb +
               BATL23_other 

    
	batl_total_incomplete <- sum(c(BATL1_crash ,BATL2_motor ,BATL3_terr ,
	                               BATL4_ped ,BATL5_obj ,BATL6_equip ,
	                               BATL7_stairs ,BATL8_high ,BATL9_faint ,
	                               BATL10_drug , BATL11_bike ,BATL12_roll ,
	                               BATL13_horse ,BATL14_ski ,BATL15_sky ,
	                               BATL16_sport ,BATL17_play ,BATL18_water ,
	                               BATL19_abuse ,BATL20_mugg ,BATL21_mil ,
	                               BATL22_comb ,BATL23_other),na.rm=T)
                    
   severe_tbi <- as.numeric(BATL25_time == 4)
   batl_quality_flag = NA
   if (is.na(BATL24_worst) | is.na(BATL25_time))
   {
   	batl_quality_flag = NA
   } else if(BATL24_worst == 0 & BATL25_time > 0 )
   {
     batl_quality_flag = 1
   } else if (BATL24_worst >  0 & BATL25_time > 0 )
   {
   	batl_quality_flag = 0
   } else if (BATL24_worst == 0 & BATL25_time == 0 )
   {
   	batl_quality_flag = 0
   } 
  
   data_complete_batl<- as.numeric(
     sum(
       is.na(
         c(BATL1_crash ,
             BATL2_motor ,
             BATL3_terr ,
             BATL4_ped ,
             BATL5_obj ,
             BATL6_equip ,
             BATL7_stairs ,
             BATL8_high ,
             BATL9_faint ,
             BATL10_drug ,
             BATL11_bike ,
             BATL12_roll ,
             BATL13_horse ,
             BATL14_ski ,
             BATL15_sky ,
             BATL16_sport ,
             BATL17_play ,
             BATL18_water ,
             BATL19_abuse ,
             BATL20_mugg ,
             BATL21_mil ,
             BATL22_comb ,
             BATL23_other
         )
       )
     ) == 0
   )
                
    scores <- data.frame(batl_total,severe_tbi,batl_quality_flag, batl_total_incomplete, data_complete_batl)
    
	return(scores)
}


#Calculate summary scores in data 
 batl_scores <- adply(dat_BATL, 1, batl)

#Export data
 write.csv( batl_scores, "~/Biobank/3_BAT-L/batl_reduced_data_export_20171221.csv",quote=T,row.names=F,na="#N/A")


