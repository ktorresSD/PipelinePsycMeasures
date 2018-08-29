#########################################################################################
# Last Date modified: 12/21/2017
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
                        Stat10_bed_wakeupnight))
                        
#________________________________________________________________________________________              
# Completeness Functions Defined
#----------------------------------------------------------------------------------------
score_brief <- function(x)
{          
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
  
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
      
            
  scores <- data.frame(completeness_brief)
  
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

# #________________________________________________________________________________________ 
# #Export datBTBISa
# #----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/7_Brief_CESAMH_Biorepository_Survey/Brief_CESAMH_reduced_data_export.csv", sep="")
write.csv(score_datbrief, filename,quote=T,row.names=F,na="#N/A")

filename <- paste("~/Biobank/7_Brief_CESAMH_Biorepository_Survey/Brief_CESAMH_reduced_data_export_DEIDENTIFIED.csv", sep="")
write.csv(score_datbrief1, filename,quote=T,row.names=F,na="#N/A")

print("7_Brief_done")

#return completness column
myvars <- c("assessment_id", "completeness_brief")
newdata <- score_datbrief[myvars]
return(newdata)
}




