#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 20, PCL LIFETIME
##########################################################################################

#Load plyr library
 library(plyr)

#To the user: Set path to where data is stored
 setwd('C:/Users/Psychiatry Lab/Documents/Biobank/data')
#setwd('C:/users/adam/Desktop/')

#Read all data
 dat0 <- read.csv('joined_data_export_20180328.csv',header=T,na.strings=c(NA,999))
 
#Only retain relevant variables
 datpcllife <- subset(dat0, 
               select= c(assessment_id,vista_lastname, 
                         pcl5_entirelife_1_memories,
                    pcl5_entirelife_2_dream,
                    pcl5_entirelife_3_acting,
                    pcl5_entirelife_4_upset,
                    pcl5_entirelife_5_physical,
                    pcl5_entirelife_6_avoid,
                    pcl5_entirelife_7_external,
                    pcl5_entirelife_8_trouble,
                    pcl5_entirelife_9_negbelief,
                    pcl5_entirelife_10_blame,
                    pcl5_entirelife_11_fear,
                    pcl5_entirelife_12_interest,
                    pcl5_entirelife_13_distant,
                    pcl5_entirelife_14_posfeel,
                    pcl5_entirelife_15_irritable,
                    pcl5_entirelife_16_risk,
                    pcl5_entirelife_17_superalert,
                    pcl5_entirelife_18_jumpy,
                    pcl5_entirelife_19_concentrate,
                    pcl5_entirelife_20_sleep))

             
#Scoring function defined
pcl_5_entire_life <- function(x)
{
    for (v in 1:length(x)) assign(names(x)[v], x[[v]])
    
	#PCL summary score is just the summation of all items 1-20
	#Note: This function is not designed to handle NA values (subject must have complete data)

	pcl_b <- pcl5_entirelife_1_memories +
        pcl5_entirelife_2_dream +
        pcl5_entirelife_3_acting +
        pcl5_entirelife_4_upset +
        pcl5_entirelife_5_physical 
        
    pcl_c <- pcl5_entirelife_6_avoid +
        pcl5_entirelife_7_external 
        
    pcl_d <-  pcl5_entirelife_8_trouble +
        pcl5_entirelife_9_negbelief +
        pcl5_entirelife_10_blame +
        pcl5_entirelife_11_fear +
        pcl5_entirelife_12_interest +
        pcl5_entirelife_13_distant +
        pcl5_entirelife_14_posfeel 
        
    pcl_e <-pcl5_entirelife_15_irritable +
        pcl5_entirelife_16_risk +
        pcl5_entirelife_17_superalert +
        pcl5_entirelife_18_jumpy +
        pcl5_entirelife_19_concentrate +
        pcl5_entirelife_20_sleep
    
    pcl_total <- pcl_b + pcl_c + pcl_d + pcl_e
    

                    
                    
   pcl_33 <- as.numeric(pcl_total >= 33)
    
   pcl_incomplete <- sum(pcl5_entirelife_1_memories,
                pcl5_entirelife_2_dream,
                pcl5_entirelife_3_acting,
                pcl5_entirelife_4_upset,
                pcl5_entirelife_5_physical,
                pcl5_entirelife_6_avoid,
                pcl5_entirelife_7_external,
                pcl5_entirelife_8_trouble,
                pcl5_entirelife_9_negbelief,
                pcl5_entirelife_10_blame,
                pcl5_entirelife_11_fear,
                pcl5_entirelife_12_interest,
                pcl5_entirelife_13_distant,
                pcl5_entirelife_14_posfeel,
                pcl5_entirelife_15_irritable,
                pcl5_entirelife_16_risk,
                pcl5_entirelife_17_superalert,
                pcl5_entirelife_18_jumpy,
                pcl5_entirelife_19_concentrate,
                pcl5_entirelife_20_sleep,na.rm=T)
                
    if(pcl_incomplete >= 33)
    {
     pcl_33 <- 1
    }
   
    
    ##PCL B 
    #Assign TRUE to each PCL B item score that is >= 2
    pcl_5_b_gt2 <- c(pcl5_entirelife_1_memories,pcl5_entirelife_2_dream,pcl5_entirelife_3_acting, pcl5_entirelife_4_upset, pcl5_entirelife_5_physical) >= 2
    
    #Assign TRUE if at least one PCL B is >= 2
    pcl_5_b_dsm5 <- sum(pcl_5_b_gt2) >= 1
    
    ##PCL C
    #Assign TRUE to each PCL C item score that is >= 2
    pcl_5_c_gt2 <- c(pcl5_entirelife_6_avoid, pcl5_entirelife_7_external ) >= 2
    
    #Assign TRUE if at least one PCL C is >= 2
    pcl_5_c_dsm5 <- sum(pcl_5_c_gt2) >= 1
    
    ##PCL D 
    #Assign TRUE to each PCL D item score that is >= 2
    pcl_5_d_gt2 <- c(pcl5_entirelife_8_trouble ,
        pcl5_entirelife_9_negbelief ,
        pcl5_entirelife_10_blame ,
        pcl5_entirelife_11_fear ,
        pcl5_entirelife_12_interest ,
        pcl5_entirelife_13_distant ,
        pcl5_entirelife_14_posfeel  ) >= 2
    
    #Assign TRUE if at least two PCL Ds are >= 2
    pcl_5_d_dsm5 <- sum(pcl_5_d_gt2) >= 2
    
    ##PCL E 
    #Assign TRUE to each PCL E item score that is >= 2
    pcl_5_e_gt2 <- c(pcl5_entirelife_15_irritable ,
        pcl5_entirelife_16_risk ,
        pcl5_entirelife_17_superalert ,
        pcl5_entirelife_18_jumpy ,
        pcl5_entirelife_19_concentrate ,
        pcl5_entirelife_20_sleep  ) >= 2
      

    
    #Assign TRUE if at least two PCL Es are >= 2
    pcl_5_e_dsm5 <- sum(pcl_5_e_gt2) >= 2
    
    #Assign TRUE if all PCL sub-symptoms are TRUE
    pcl_5_dsm <- as.numeric(
                    sum(pcl_5_b_dsm5, pcl_5_c_dsm5, pcl_5_d_dsm5, pcl_5_e_dsm5) == 4
                        )
    
    ###Infer DSM if data is incomplete
    ##PCL B 
    #Assign TRUE to each PCL B item score that is >= 2
    pcl_5_b_gt2_infer <- na.omit(c(pcl5_entirelife_1_memories,pcl5_entirelife_2_dream,pcl5_entirelife_3_acting, pcl5_entirelife_4_upset, pcl5_entirelife_5_physical)) >= 2
    
    #Assign TRUE if at least one PCL B is >= 2
    pcl_5_b_dsm5_infer <- sum(pcl_5_b_gt2_infer) >= 1
    
    ##PCL C
    #Assign TRUE to each PCL C item score that is >= 2
    pcl_5_c_gt2_infer <- na.omit(c(pcl5_entirelife_6_avoid, pcl5_entirelife_7_external )) >= 2
    
    #Assign TRUE if at least one PCL C is >= 2
    pcl_5_c_dsm5_infer <- sum(pcl_5_c_gt2_infer) >= 1
    
    ##PCL D 
    #Assign TRUE to each PCL D item score that is >= 2
    pcl_5_d_gt2_infer <- na.omit(c(pcl5_entirelife_8_trouble ,
        pcl5_entirelife_9_negbelief ,
        pcl5_entirelife_10_blame ,
        pcl5_entirelife_11_fear ,
        pcl5_entirelife_12_interest ,
        pcl5_entirelife_13_distant ,
        pcl5_entirelife_14_posfeel  )) >= 2
    
    #Assign TRUE if at least two PCL Ds are >= 2
    pcl_5_d_dsm5_infer <- sum(pcl_5_d_gt2_infer) >= 2
    
    ##PCL E 
    #Assign TRUE to each PCL E item score that is >= 2
    pcl_5_e_gt2_infer <- na.omit(c(pcl5_entirelife_15_irritable,
        pcl5_entirelife_16_risk ,
        pcl5_entirelife_17_superalert ,
        pcl5_entirelife_18_jumpy ,
        pcl5_entirelife_19_concentrate ,
        pcl5_entirelife_20_sleep  )) >= 2
      

    
    #Assign TRUE if at least two PCL Es are >= 2
    pcl_5_e_dsm5_infer <- sum(pcl_5_e_gt2_infer) >= 2
    
    #Assign TRUE if all PCL sub-symptoms are TRUE
    pcl_5_dsm_infer <- as.numeric(
                    sum(pcl_5_b_dsm5_infer, pcl_5_c_dsm5_infer, pcl_5_d_dsm5_infer, pcl_5_e_dsm5_infer) == 4
                        )
    if(pcl_5_dsm_infer == TRUE)
    {
     pcl_5_dsm = 1
    }    

    
    data_complete_pcl_life <- as.numeric( 
                sum(
                    is.na(
                    c(pcl5_entirelife_1_memories,
                    pcl5_entirelife_2_dream,
                    pcl5_entirelife_3_acting,
                    pcl5_entirelife_4_upset,
                    pcl5_entirelife_5_physical,
                    pcl5_entirelife_6_avoid,
                    pcl5_entirelife_7_external,
                    pcl5_entirelife_8_trouble,
                    pcl5_entirelife_9_negbelief,
                    pcl5_entirelife_10_blame,
                    pcl5_entirelife_11_fear,
                    pcl5_entirelife_12_interest,
                    pcl5_entirelife_13_distant,
                    pcl5_entirelife_14_posfeel,
                   pcl5_entirelife_15_irritable,
                    pcl5_entirelife_16_risk,
                    pcl5_entirelife_17_superalert,
                    pcl5_entirelife_18_jumpy,
                    pcl5_entirelife_19_concentrate,
                    pcl5_entirelife_20_sleep)
                    )
                ) == 0
                )
                
    scorespcllife <- data.frame(pcl_b,pcl_c,pcl_d,pcl_e,pcl_total,pcl_incomplete, pcl_33,pcl_5_dsm,data_complete_pcl_life)
    
	return(scorespcllife)
}


#Calculate summary scores in data 
 pcl_5_scores <- adply(datpcllife, 1, pcl_5_entire_life)

#Export data
 write.csv( pcl_5_scores, "C:/Users/Psychiatry Lab/Documents/Biobank/20_21_PCL_5/pcl5_entire_life_reduced_data_export_20171221.csv",quote=T,row.names=F,na="#N/A")


