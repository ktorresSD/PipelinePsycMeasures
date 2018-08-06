#########################################################################################
# Last Date modified: 6/21/2018
# Author: Katy Torres
# Description: Subset of question 20, PCL Current
##########################################################################################
pclcurrent<- function(dat0, exportdate)
{
  
#Load plyr library
 library(plyr)


#Only retain relevant variables
 datpclcurr <- subset(dat0, 
               select= c(assessment_id,vista_lastname,visit_number,
                    pcl5_m_1_memories,
                    pcl5_m_2_dream,
                    pcl5_m_3_acting,
                    pcl5_m_4_upset,
                    pcl5_m_5_physical,
                    pcl5_m_6_avoid,
                    pcl5_m_7_external,
                    pcl5_m_8_trouble,
                    pcl5_m_9_negbelief,
                    pcl5_m_10_blame,
                    pcl5_m_11_fear,
                    pcl5_m_12_interest,
                    pcl5_m_13_distant,
                    pcl5_m_14_posfeel,
                    pcl5_m_15_irritable,
                    pcl5_m_16_risk,
                    pcl5_m_17_superalert,
                    pcl5_m_18_jumpy,
                    pcl5_m_19_concentrate,
                    pcl5_m_20_sleep))

             
#Scoring function defined
pcl_5_current <- function(x)
{

    #attach(x)
    for (v in 1:length(x)) assign(names(x)[v], x[[v]])
    
	#PCL summary score is just the summation of all items 1-20
	#Note: This function is not designed to handle NA values (subject must have complete data)

                
  #sum of items 1-5            
	 pcl_b <- pcl5_m_1_memories +
        pcl5_m_2_dream +
        pcl5_m_3_acting +
        pcl5_m_4_upset +
        pcl5_m_5_physical 
	
  #sum of items 6 and 7
    pcl_c <- pcl5_m_6_avoid +
        pcl5_m_7_external 
    
  #sum of items 8-14    
    pcl_d <-  pcl5_m_8_trouble +
        pcl5_m_9_negbelief +
        pcl5_m_10_blame +
        pcl5_m_11_fear +
        pcl5_m_12_interest +
        pcl5_m_13_distant +
        pcl5_m_14_posfeel 
        
  #sum of items 15-20
    pcl_e <- pcl5_m_15_irritable +
        pcl5_m_16_risk +
        pcl5_m_17_superalert +
        pcl5_m_18_jumpy +
        pcl5_m_19_concentrate +
        pcl5_m_20_sleep
    
   #total symptom severity score (range - 0-80). Obtained by summing the scores for each of the 20 items
    pcl_total <- pcl_b + pcl_c + pcl_d + pcl_e
    
   #sum of all non-na entries for questions 1-20  
    pcl_incomplete <- sum(pcl5_m_1_memories,
                          pcl5_m_2_dream,
                          pcl5_m_3_acting,
                          pcl5_m_4_upset,
                          pcl5_m_5_physical,
                          pcl5_m_6_avoid,
                          pcl5_m_7_external,
                          pcl5_m_8_trouble,
                          pcl5_m_9_negbelief,
                          pcl5_m_10_blame,
                          pcl5_m_11_fear,
                          pcl5_m_12_interest,
                          pcl5_m_13_distant,
                          pcl5_m_14_posfeel,
                          pcl5_m_15_irritable,
                          pcl5_m_16_risk,
                          pcl5_m_17_superalert,
                          pcl5_m_18_jumpy,
                          pcl5_m_19_concentrate,
                          pcl5_m_20_sleep,na.rm=T)
                    
  #A PCL-5 cutpoint score of 33 appears to be a reasonable value to propose until further psychometric work is available
   pcl_33 <- as.numeric(pcl_total >= 33)
          
    if(pcl_incomplete >= 33)
    {
     pcl_33 <- 1
    }
   
    
    ##PCL B 
    #Assign TRUE to each PCL B item score that is >= 2
    pcl_5_b_gt2 <- c(pcl5_m_1_memories,pcl5_m_2_dream,pcl5_m_3_acting, pcl5_m_4_upset, pcl5_m_5_physical) >= 2
    
    #Assign TRUE if at least one PCL B is >= 2
    pcl_5_b_dsm5 <- sum(pcl_5_b_gt2) >= 1
    
    
    ##PCL C
    #Assign TRUE to each PCL C item score that is >= 2
    pcl_5_c_gt2 <- c(pcl5_m_6_avoid, pcl5_m_7_external ) >= 2
    
    #Assign TRUE if at least one PCL C is >= 2
    pcl_5_c_dsm5 <- sum(pcl_5_c_gt2) >= 1
    
    
    ##PCL D 
    #Assign TRUE to each PCL D item score that is >= 2
    pcl_5_d_gt2 <- c(pcl5_m_8_trouble ,
        pcl5_m_9_negbelief ,
        pcl5_m_10_blame ,
        pcl5_m_11_fear ,
        pcl5_m_12_interest ,
        pcl5_m_13_distant ,
        pcl5_m_14_posfeel  ) >= 2
    
    #Assign TRUE if at least two PCL Ds are >= 2
    pcl_5_d_dsm5 <- sum(pcl_5_d_gt2) >= 2
    
    
    ##PCL E 
    #Assign TRUE to each PCL E item score that is >= 2
    pcl_5_e_gt2 <- c(pcl5_m_15_irritable ,
        pcl5_m_16_risk ,
        pcl5_m_17_superalert ,
        pcl5_m_18_jumpy ,
        pcl5_m_19_concentrate ,
        pcl5_m_20_sleep  ) >= 2
      
    #Assign TRUE if at least two PCL Es are >= 2
    pcl_5_e_dsm5 <- sum(pcl_5_e_gt2) >= 2
    
    
    #DSM-5 symptom cluster severity scores can be obtained by summing the
    #scores for the items within a given cluster
    #Assign TRUE if all PCL sub-symptoms are TRUE
    pcl_5_dsm <- as.numeric(
                    sum(pcl_5_b_dsm5, pcl_5_c_dsm5, pcl_5_d_dsm5, pcl_5_e_dsm5) == 4
                        )
    
    #flag checks if data has been entered for all 20 questions
    data_complete_pcl_curr <- as.numeric( 
      sum(
        is.na(
          c(pcl5_m_1_memories,
            pcl5_m_2_dream,
            pcl5_m_3_acting,
            pcl5_m_4_upset,
            pcl5_m_5_physical,
            pcl5_m_6_avoid,
            pcl5_m_7_external,
            pcl5_m_8_trouble,
            pcl5_m_9_negbelief,
            pcl5_m_10_blame,
            pcl5_m_11_fear,
            pcl5_m_12_interest,
            pcl5_m_13_distant,
            pcl5_m_14_posfeel,
            pcl5_m_15_irritable,
            pcl5_m_16_risk,
            pcl5_m_17_superalert,
            pcl5_m_18_jumpy,
            pcl5_m_19_concentrate,
            pcl5_m_20_sleep)
        )
      ) == 0
    )
    
    data_not_attempted_pcl_curr <- as.numeric( 
      sum(
        is.na(
          c(pcl5_m_1_memories,
            pcl5_m_2_dream,
            pcl5_m_3_acting,
            pcl5_m_4_upset,
            pcl5_m_5_physical,
            pcl5_m_6_avoid,
            pcl5_m_7_external,
            pcl5_m_8_trouble,
            pcl5_m_9_negbelief,
            pcl5_m_10_blame,
            pcl5_m_11_fear,
            pcl5_m_12_interest,
            pcl5_m_13_distant,
            pcl5_m_14_posfeel,
            pcl5_m_15_irritable,
            pcl5_m_16_risk,
            pcl5_m_17_superalert,
            pcl5_m_18_jumpy,
            pcl5_m_19_concentrate,
            pcl5_m_20_sleep)
        )
      ) == 20
    )
    
    completeness_pcl_curr<- "1"
    if(!(is.na(data_not_attempted_pcl_curr))){
      if(data_not_attempted_pcl_curr==1)
      {
        completeness_pcl_curr <- "not attempted"}else{}
    }else{completeness_pcl_curr<-NA}
    
    if(!(is.na(data_complete_pcl_curr))){
      if(data_complete_pcl_curr==1){
        completeness_pcl_curr <- "complete"} else{}
    }else{completeness_pcl_curr<-NA}
    
    
    if(data_not_attempted_pcl_curr==0 & data_complete_pcl_curr==0){
      completeness_pcl_curr <- "partially completed"}else{}
    
    
    ###Infer DSM if data is incomplete
    ##PCL B
    #Assign TRUE to each PCL B item score that is >= 2
    pcl_5_b_gt2_infer <- na.omit(c(pcl5_m_1_memories,pcl5_m_2_dream,pcl5_m_3_acting, pcl5_m_4_upset, pcl5_m_5_physical)) >= 2

    #Assign TRUE if at least one PCL B is >= 2
    pcl_5_b_dsm5_infer <- sum(pcl_5_b_gt2_infer) >= 1

    ##PCL C
    #Assign TRUE to each PCL C item score that is >= 2
    pcl_5_c_gt2_infer <- na.omit(c(pcl5_m_6_avoid, pcl5_m_7_external )) >= 2

    #Assign TRUE if at least one PCL C is >= 2
    pcl_5_c_dsm5_infer <- sum(pcl_5_c_gt2_infer) >= 1

    ##PCL D
    #Assign TRUE to each PCL D item score that is >= 2
    pcl_5_d_gt2_infer <- na.omit(c(pcl5_m_8_trouble ,
        pcl5_m_9_negbelief ,
        pcl5_m_10_blame ,
        pcl5_m_11_fear ,
        pcl5_m_12_interest ,
        pcl5_m_13_distant ,
        pcl5_m_14_posfeel  )) >= 2

    #Assign TRUE if at least two PCL Ds are >= 2
    pcl_5_d_dsm5_infer <- sum(pcl_5_d_gt2_infer) >= 2

    ##PCL E
    #Assign TRUE to each PCL E item score that is >= 2
    pcl_5_e_gt2_infer <- na.omit(c(pcl5_m_15_irritable ,
        pcl5_m_16_risk ,
        pcl5_m_17_superalert ,
        pcl5_m_18_jumpy ,
        pcl5_m_19_concentrate ,
        pcl5_m_20_sleep  )) >= 2



    #Assign TRUE if at least two PCL Es are >= 2
    pcl_5_e_dsm5_infer <- sum(pcl_5_e_gt2_infer) >= 2

    #Assign TRUE if all PCL sub-symptoms are TRUE
    pcl_5_dsm_infer <- as.numeric(
                    sum(pcl_5_b_dsm5_infer, pcl_5_c_dsm5_infer, pcl_5_d_dsm5_infer, pcl_5_e_dsm5_infer) == 4
                        )
    # if(pcl_5_dsm_infer == TRUE)
    # {
    #  pcl_5_dsm = 1
    # }    
    # 
                
    scores <- data.frame(pcl_b,pcl_c,pcl_d,pcl_e,pcl_total,pcl_33,pcl_5_dsm,  pcl_5_dsm_infer, data_complete_pcl_curr, data_not_attempted_pcl_curr, completeness_pcl_curr)
    
	return(scores)
}


#Calculate summary scores in data 
 pcl_5_scorescurr <- adply(datpclcurr, 1, pcl_5_current)
 
 #to anonymize data
 pcl_5_scorescurr1<- within(pcl_5_scorescurr,
                         {
                           assessment_id <- NULL
                           vista_lastname <- NULL
                         })

 #________________________________________________________________________________________ 
 #Export
 #----------------------------------------------------------------------------------------
 filename <- paste("~/Biobank/21_PCL_5_monthly/pcl5_current_scored_data_export.csv", sep="")
 write.csv( pcl_5_scorescurr, filename,quote=T,row.names=F,na="#N/A")
 
 
 filename <- paste("~/Biobank/21_PCL_5_monthly/pcl5_current_scored_data_export_DEIDENTIFIED.csv", sep="")
 write.csv( pcl_5_scorescurr1, filename,quote=T,row.names=F,na="#N/A")
 
print("21_PCL_current_done")

#return completness column
myvars <- c("assessment_id", "completeness_pcl_curr")
newdata <- pcl_5_scorescurr[myvars]
return(newdata)
}





