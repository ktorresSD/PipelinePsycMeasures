#########################################################################################
# Last Date modified: 10/22/2018
# Author: Katy Torres
# Description: Subset of question 9, CURRENT TREATMENTS
##########################################################################################
currtx <- function(dat0, exportdate)
{

#Only retain relevant variables
 datcurrent <- subset(dat0, 
               select= c(assessment_id,vista_lastname,
                         CurrentTreatments1_MHTx,	
                         CurrTx2a_Dep,	CurrTx2b_Anx,	
                         CurrTx2c_PTSD,	CurrTx2d_Schiz,	
                         CurrTx2e_BP,	
                         CurrTx2f_Subst,	 CurrTx2g_Other,	
                         CurrentTreatments3_VA,	
                         CurrTx4a_LJBHIP,	
                         CurrTx4b_LJPTSD,	
                         CurrTx4c_LJMood,	
                         CurrTx4d_Other,	
                         CurrTx5a_AntiD,
                         CurrTx5b_Mood,	
                         CurrTx5c_Stim,
                         CurrTx5d_Sleep,
                         CurrTx5e_Benzo,	
                         CurrTx5f_AntiPsy,
                         CurrTx5g_Adren, 
                         CurrTx5h_Other, 
                         CurrTx5.1_VAProvider, 
                         CurrTx5.2_Helpful, 
                         CurrTx5.3_SideEffects, 
                         CurrentTreatments6_Psycho, 
                         CurrTx6.1_Group, 
                         CurrTx6.1_Ind, 
                         CurrTx6.1_Fam, 
                         CurrTx6.1_Couples, 
                         CurrTx6.2_AtVA, 
                         CurrTx7a_CBTAnx, 
                         CurrTx7b_CBTDep, 
                         CurrTx7c_CBTInsom, 
                         CurrTx7d_IRTNM, 
                         CurrTx7e_CBTBP, 
                         CurrTx7f_CPT, 
                         CurrTx7g_Anger, 
                         CurrTx7h_ACT, 
                         CurrTx7i_Other, 
                         CurrTx7j_None, #new
                         
                         CurrTx7.1_Helpful, #might remove
                         CurrTx8.1_Helpful, 
                         CurrTx8a_CBTAnx, 
                         CurrTx8b_CBTDep, 
                         CurrTx8c_CBTInsom, 
                         CurrTx8d_IRTNM, 
                         CurrTx8e_CPT, 
                         CurrTx8f_PE, 
                         CurrTx8g_EMDR, 
                         CurrTx8h_ACT, 
                         CurrTx8g_Anger, 
                         CurrTx8i_Other, 
                         CurrTx8j_None, #new
                         
                         CurrTx8.1_Helpful, 
                         CurrentTreatments9_OtherType
               ))
              
 #to anonymize data
 datcurrent1<- within(datcurrent,
                           {
                             assessment_id <- NULL
                             vista_lastname <- NULL
                           })
 #________________________________________________________________________________________ 
 #Export
 #----------------------------------------------------------------------------------------
 filename <- paste("~/Biobank/99_Current_treatments_paper_form/9_current_treatments/CurrTx_reduced_data_export.csv", sep="")
 write.csv(datcurrent, filename,quote=T,row.names=F,na="#N/A")
 
 filename <- paste("~/Biobank/99_Current_treatments_paper_form/9_current_treatments/CurrTx_reduced_data_export_DEIDENTIFIED.csv", sep="")
 write.csv(datcurrent1, filename,quote=T,row.names=F,na="#N/A")
 
 return(print("9_current_treatment_done"))
}

