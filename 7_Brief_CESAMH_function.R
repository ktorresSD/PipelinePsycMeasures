#########################################################################################
# Last Date modified: 10/22/2018
# Author: Katy Torres
# Description: Subset of question 7, Brief CESAMH Biorepository Sample Status Survey
##########################################################################################
briefCESAM<- function(dat0, exportdate)
{
#Load plyr library
library(plyr)
  
#Only retain relevant variables
datcesamh<- subset(dat0, 
              select= c(assessment_id,vista_lastname,visit_number,
                        Stat1_cold,
                        Stat2_nic,
                        
                        Stat2_nicroute_chew,
                        Stat2_nicroute_cig,
                        Stat2_nicroute_vap,
                        Stat2_nicroute_inh,
                        Stat2_nicroute_pat,
                        Stat2_nictime,
                        Stat3_eat,
                        Stat4_caff,
                        Stat5_alc,
                        
                        Stat6_diet,
                        Stat6_diet_spec,
                        Stat7_med_blood,
                        Stat7_med_blood_spec,
                        Stat7_med_statin,
                        Stat7_med_statin_spec,
                        Stat7_med_heart,
                        Stat7_med_heart_spec,
                        Stat7_med_diabetes,
                        Stat7_med_diabetes_spec,
                        Stat7_med_psych,
                        Stat7_med_psych_spec,
                        Stat7_med_pain,
                        Stat7_med_pain_spec,
                        Stat7_med_sleep,
                        Stat7_med_sleep_spec,
                        Stat7_med_antibiotics,
                        Stat7_med_antibotics_spec,
                        Stat7_med_prost,
                        Stat7_med_prost_spec,
                        Stat7_med_allerg,
                        Stat7_med_allerg_spec,
                        Stat7_med_steroid,
                        Stat7_med_steroid_spec,
                        Stat7_med_none,
                        
                        Stat8_marijtoday,
                        Stat9_cannabisweek,
                        
                        Stat10_bed_night,
                        Stat10_bed_night_ampm,
                        Stat10_bed_morn,
                        Stat10_bed_morn_ampm,
                        Stat10_bed_wakeupnight,
                        Stat10_bed_night_ampm,
                        Stat10_bed_morn_ampm
              ))
                        
#________________________________________________________________________________________              
# Completeness Functions Defined
#----------------------------------------------------------------------------------------
score_brief <- function(x)
{          
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
  
  if(!(is.na(Stat2_nic))){
    if(Stat2_nic==0)
    {
      nicotine_today <- 0}else{nicotine_today <- 1}
  }else{nicotine_today<-NA}


  if(!(is.na(Stat2_nic | Stat2_nicroute_chew | Stat2_nicroute_cig |Stat2_nicroute_vap| Stat2_nicroute_inh |  Stat2_nicroute_pat))){
    if(Stat2_nic== 0 & (Stat2_nicroute_chew==1 | Stat2_nicroute_cig==1 |Stat2_nicroute_vap==1 | Stat2_nicroute_inh==1 | Stat2_nicroute_pat ==1 )){nicotine_flag_conflicting <- 1}else{nicotine_flag_conflicting<- 0}
  }else{nicotine_flag_conflicting<-NA}


  
  
  data_complete_minus_meds_brief<- as.numeric(
              sum(
                is.na(
                  c(Stat1_cold,Stat2_nic,Stat3_eat, Stat4_caff, Stat5_alc,Stat6_diet,
                    Stat8_marijtoday,
                    Stat9_cannabisweek,
                    Stat10_bed_night,
                    Stat10_bed_morn,
                    Stat10_bed_wakeupnight
                  )
                )
              ) == 0
            )
            
  meds_complete<- as.numeric(
    sum(
      is.na(
        c(Stat7_med_blood,
          Stat7_med_statin,
          Stat7_med_heart,
          Stat7_med_diabetes,
          Stat7_med_psych,
          Stat7_med_pain,
          Stat7_med_sleep,
          Stat7_med_antibiotics,
          Stat7_med_prost,
          Stat7_med_allerg,
          Stat7_med_steroid
        )
      )
    ) == 0
  )
  
  data_not_attempted_brief<- as.numeric(
    sum(
      is.na(
        c(Stat1_cold,Stat2_nic,Stat3_eat, Stat4_caff, Stat5_alc,Stat6_diet,
          Stat7_med_blood,
          Stat7_med_statin,
          Stat7_med_heart,
          Stat7_med_diabetes,
          Stat7_med_psych,
          Stat7_med_pain,
          Stat7_med_sleep,
          Stat7_med_antibiotics,
          Stat7_med_prost,
          Stat7_med_allerg,
          Stat7_med_steroid,
          Stat8_marijtoday,
          Stat9_cannabisweek,
          Stat10_bed_night,
          Stat10_bed_morn,
          Stat10_bed_wakeupnight
        )
      )
    ) == 22
  )
            
      completeness_brief<- "1"
      if(!(is.na(data_not_attempted_brief))){
        if(data_not_attempted_brief==1)
        {
          completeness_brief <- "not attempted"}else{}
      }else{completeness_brief<-NA}
      
      if(!(is.na(data_complete_minus_meds_brief))){
        if(data_complete_minus_meds_brief==1 & meds_complete == 1){
          completeness_brief <- "complete with meds"} else{}
      }else{completeness_brief<-NA}
      
      if(!(is.na(data_complete_minus_meds_brief))){
          if(data_complete_minus_meds_brief==1 & meds_complete == 0){
             completeness_brief <- "complete with no meds"} 
          else{}
          
          # else if(data_complete_minus_meds_brief==0 & meds_complete == 1){
          #     completeness_brief <- "partially completed"}else{}
      }else{completeness_brief<-NA}

      
      if(!(is.na(data_complete_minus_meds_brief))){
        if(data_not_attempted_brief == 0 & data_complete_minus_meds_brief==0){
          completeness_brief <- "partially completed"}else{}
      }else{completeness_brief<-NA}
      
            
  scores <- data.frame(nicotine_flag_conflicting,  completeness_brief)
  
  return(scores)
}               

#Calculate summary scores in datBTBISa
score_datbrief <- adply(datcesamh, 1, score_brief)

#to anonymize data
score_datbrief1<- within(score_datbrief,
                  {
                    assessment_id <- NULL
                    vista_lastname <- NULL
                  })

# #bring in CDDR data to check correlation between smoking cigarretes and smoking cannabis
# cddr <- read.csv("C:/Users/Nievergelt Lab/Documents/Biobank/00_Freeze_1_2018_data/scored_data_from_eScreening_modules/CDDR_reduced_data_export_combined.csv",header=T,na.strings=c("#N/A",NA))
# vars<-c("assessment_id", "vista_lastname","visit_number", "CDDR2_EverSmoked")
# cddrsub<- cddr[vars]
# dat <- merge(score_datbrief, cddrsub, by=c("assessment_id", "vista_lastname", "visit_number"), all = FALSE)
# 
# table(dat$CDDR2_EverSmoked)
# chisq.test(dat$Stat2_nic,dat$CDDR2_EverSmoked)

# #________________________________________________________________________________________ 
# #Export datBTBISa
# #----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/7_Brief_CESAMH_Biorepository_Survey/Brief_CESAMH_reduced_data_export.csv", sep="")
write.csv(score_datbrief, filename,quote=T,row.names=F,na="NA")

filename <- paste("~/Biobank/7_Brief_CESAMH_Biorepository_Survey/Brief_CESAMH_reduced_data_export_DEIDENTIFIED.csv", sep="")
write.csv(score_datbrief1, filename,quote=T,row.names=F,na="NA")

print("7_Brief_done")

#return completness column
myvars <- c("assessment_id", "completeness_brief")
newdata <- score_datbrief[myvars]
return(newdata)
}





