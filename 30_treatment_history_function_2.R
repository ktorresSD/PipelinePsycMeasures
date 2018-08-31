#########################################################################################
# Last Date modified: 06/27/2017
# Author: Katy Torres
# Description: Subset of question 30, Treatment History
##########################################################################################
treathist<- function(dat0, exportdate)
{
dat<-dat0[dat0$visit_number==1,]

#Only retain relevant variables
dattreatment <- subset(dat, 
              select= c(assessment_id,vista_lastname, visit_number,
                        TreatHis_1_treatment,
                        TreatHis_2_depression,
                        TreatHis_2_anxiety,
                        TreatHis_2_ptsd,
                        TreatHis_2_schizo,
                        TreatHis_2_bipolar,
                        TreatHis_2_substance,
                        TreatHis_2_other,
                        TreatHis_2_none,
                        TreatHis_3_VAtreat,
                        TreatHis_4_clinic,
                        TreatHis_5a_Antidep,
                        TreatHis_5b_moodstab,
                        TreatHis_5c_stimulant,
                        TreatHis_5d_sleepaid,
                        TreatHis_5e_Benzo,
                        TreatHis_5f_Antipsy,
                        TreatHis_5g_AdBlk,
                        TreatHis_5h_other,
                        TreatHis_6_presc,
                        TreatHis_7_helpful,
                        TreatHis_8_discont,
                        TreatHis_9_Psycho,
                        TreatHis_9_VAtherap,
                        TreatHis2_9c_GpTher,
                        TreatHis2_9c_IndTher,
                        TreatHis2_9c_FamTher,
                        TreatHis2_9c_CoCoun,
                        TreatHis2_9c_None,
                        TreatHis_10_gpdates,
                        TreatHis_10gpdateend,
                        TreatHis_10_gphelp,
                        TreatHis_10_gpcomp,
                        TreatHis_10_gpCBTA,
                        TreatHis_10_gpCBTD,
                        TreatHis_10_gpCBTI,
                        TreatHis_10_gpIRTNM,
                        TreatHis_10_gpCBTBP,
                        TreatHis_10_gpCPT,
                        TreatHis_10_gpAM,
                        TreatHis_10_gpACT,
                        TreatHis_10_gpother,
                        TreatHis_10_gpnone,
                        TreatHis_11_indCBTA,
                        TreatHis_11_indCBTD,
                        TreatHis_11_indCBTI,
                        TreatHis_11_indIRTNM,
                        TreatHis_11_indCPT,
                        TreatHis_11_indPE,
                        TreatHis_11_indEMDR,
                        TreatHis_11_indACT,
                        TreatHis_11_indAM,
                        TreatHis_11_indother,
                        #TreatHis_11_indnone,
                        TreatHis_11_inddates,
                        #TreatHis_11_inddateend,
                        TreatHis_11_indhelp,
                        TreatHis_11_indcomp,
                        TreatHis_12_OtherTX,
                        TreatHis_12_OtherDate
              ))


#________________________________________________________________________________________
# Completeness check
#----------------------------------------------------------------------------------------
treath_comp <- function(x)
{
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
  
  if(!(is.na(TreatHis_1_treatment))){
    if(TreatHis_1_treatment==0)
    {
      recieve_treat <- 0}else{recieve_treat <- 1}
  }else{recieve_treat<-NA}

#checking for completeness
data_complete_treath<- as.numeric(
  sum(
    is.na(
      c(TreatHis_1_treatment,
  TreatHis_2_depression,
  TreatHis_2_anxiety,
  TreatHis_2_ptsd,
  TreatHis_2_schizo,
  TreatHis_2_bipolar,
  TreatHis_2_substance,
  TreatHis_2_other,
  TreatHis_2_none,
  TreatHis_3_VAtreat,
  TreatHis_4_clinic,
  TreatHis_5a_Antidep,
  TreatHis_5b_moodstab,
  TreatHis_5c_stimulant,
  TreatHis_5d_sleepaid,
  TreatHis_5e_Benzo,
  TreatHis_5f_Antipsy,
  TreatHis_5g_AdBlk,
  TreatHis_5h_other,
  TreatHis_6_presc,
  TreatHis_7_helpful,
  TreatHis_8_discont,
  TreatHis_9_Psycho,
  TreatHis_9_VAtherap,
  TreatHis2_9c_GpTher,
  TreatHis2_9c_IndTher,
  TreatHis2_9c_FamTher,
  TreatHis2_9c_CoCoun,
  TreatHis2_9c_None,
  TreatHis_10_gpdates,
  TreatHis_10gpdateend,
  TreatHis_10_gphelp,
  TreatHis_10_gpcomp,
  TreatHis_10_gpCBTA,
  TreatHis_10_gpCBTD,
  TreatHis_10_gpCBTI,
  TreatHis_10_gpIRTNM,
  TreatHis_10_gpCBTBP,
  TreatHis_10_gpCPT,
  TreatHis_10_gpAM,
  TreatHis_10_gpACT,
  TreatHis_10_gpother,
  TreatHis_10_gpnone,
  TreatHis_11_indCBTA,
  TreatHis_11_indCBTD,
  TreatHis_11_indCBTI,
  TreatHis_11_indIRTNM,
  TreatHis_11_indCPT,
  TreatHis_11_indPE,
  TreatHis_11_indEMDR,
  TreatHis_11_indACT,
  TreatHis_11_indAM,
  TreatHis_11_indother,
  #TreatHis_11_indnone,
  TreatHis_11_inddates,
  #TreatHis_11_inddateend,
  TreatHis_11_indhelp,
  TreatHis_11_indcomp,
  TreatHis_12_OtherTX,
  TreatHis_12_OtherDate
  
      )
    )
  ) == 0
)

  
data_not_attempted_treath<- as.numeric(
  sum(
    is.na(
      c(TreatHis_1_treatment,
        TreatHis_2_depression,
        TreatHis_2_anxiety,
        TreatHis_2_ptsd,
        TreatHis_2_schizo,
        TreatHis_2_bipolar,
        TreatHis_2_substance,
        TreatHis_2_other,
        TreatHis_2_none,
        TreatHis_3_VAtreat,
        TreatHis_4_clinic,
        TreatHis_5a_Antidep,
        TreatHis_5b_moodstab,
        TreatHis_5c_stimulant,
        TreatHis_5d_sleepaid,
        TreatHis_5e_Benzo,
        TreatHis_5f_Antipsy,
        TreatHis_5g_AdBlk,
        TreatHis_5h_other,
        TreatHis_6_presc,
        TreatHis_7_helpful,
        TreatHis_8_discont,
        TreatHis_9_Psycho,
        TreatHis_9_VAtherap,
        TreatHis2_9c_GpTher,
        TreatHis2_9c_IndTher,
        TreatHis2_9c_FamTher,
        TreatHis2_9c_CoCoun,
        TreatHis2_9c_None,
        TreatHis_10_gpdates,
        TreatHis_10gpdateend,
        TreatHis_10_gphelp,
        TreatHis_10_gpcomp,
        TreatHis_10_gpCBTA,
        TreatHis_10_gpCBTD,
        TreatHis_10_gpCBTI,
        TreatHis_10_gpIRTNM,
        TreatHis_10_gpCBTBP,
        TreatHis_10_gpCPT,
        TreatHis_10_gpAM,
        TreatHis_10_gpACT,
        TreatHis_10_gpother,
        TreatHis_10_gpnone,
        TreatHis_11_indCBTA,
        TreatHis_11_indCBTD,
        TreatHis_11_indCBTI,
        TreatHis_11_indIRTNM,
        TreatHis_11_indCPT,
        TreatHis_11_indPE,
        TreatHis_11_indEMDR,
        TreatHis_11_indACT,
        TreatHis_11_indAM,
        TreatHis_11_indother,
        #TreatHis_11_indnone,
        TreatHis_11_inddates,
        #TreatHis_11_inddateend,
        TreatHis_11_indhelp,
        TreatHis_11_indcomp,
        TreatHis_12_OtherTX,
        TreatHis_12_OtherDate
      )
    )
  ) == 60
)

completeness_treath<- "1"
if(!(is.na(data_not_attempted_treath))){
  if(data_not_attempted_treath==1)
  {
    completeness_treath <- "not attempted"}else{}
}else{completeness_treath<-NA}

if(!(is.na(data_complete_treath))){
  if(data_complete_treath==1){
    completeness_treath <- "complete"} else{}
}else{}

if(data_not_attempted_treath==0 & data_complete_treath==0){
  completeness_treath <- "partially completed"}else{}

scores <- data.frame(data_complete_treath, data_not_attempted_treath, completeness_treath )

return(scores)
}

# 
# #Calculate summary scores in data 
dattreath_scored <- adply(dattreatment, 1, treath_comp)

#to anonymize data
datreatment1<- within(dattreatment,
                        {
                          assessment_id <- NULL
                          vista_lastname <- NULL
                        })

#________________________________________________________________________________________ 
#Export
#----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/30_treatment_history/TreatHis_scored_data_export.csv", sep="")
write.csv(dattreath_scored, filename,quote=T,row.names=F,na="#N/A")

filename <- paste("~/Biobank/30_treatment_history/TreatHis_scored_data_export_DEIDENTIFIED.csv", sep="")
write.csv(dattreath_scored, filename,quote=T,row.names=F,na="#N/A")

print("30_tretment_hist_done")


#return completness column
myvars <- c("assessment_id", "completeness_treath")
newdata <- dattreath_scored[myvars]
return(newdata)

}




