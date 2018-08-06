#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 17, LEC-5 (lifetime)
##########################################################################################
leclife2<- function(dat0, exportdate)
{
#Load plyr library
library(plyr)

#Only retain relevant variables
datleclife2 <- subset(dat0, select= c(assessment_id,vista_lastname,visit_number,
                                     natdis_HAPPENED,
                                     natdis_WITNESSED,
                                     natdis_LEARNED,
                                     natdis_PART,
                                     natdis_UNSURE,
                                     natdis_DOESNTAPPLY,
                                     fire_HAPPENED,
                                     fire_WITNESSED,
                                     fire_LEARNED,
                                     fire_PART,
                                     fire_UNSURE,
                                     fire_DOESNTAPPLY,
                                     accid_HAPPENED,
                                     accid_WITNESSED,
                                     accid_LEARNED,
                                     accid_PART,
                                     accid_UNSURE,
                                     accid_DOESNTAPPLY,
                                     seriousacc_HAPPENED,
                                     seriousacc_WITNESSED,
                                     seriousacc_LEARNED,
                                     seriousacc_PART,
                                     seriousacc_UNSURE,
                                     seriousacc_DOESNTAPPLY,
                                     expos_HAPPENED,
                                     expos_WITNESSED,
                                     expos_LEARNED,
                                     expos_PART,
                                     expos_UNSURE,
                                     expos_DOESNTAPPLY,
                                     physass_HAPPENED,
                                     physass_WITNESSED,
                                     physass_LEARNED,
                                     physass_PART,
                                     physass_UNSURE,
                                     physass_DOESNTAPPLY,
                                     assweap_HAPPENED,
                                     assweap_WITNESSED,
                                     assweap_LEARNED,
                                     assweap_PART,
                                     assweap_UNSURE,
                                     assweap_DOESNTAPPLY,
                                     sexass_HAPPENED,
                                     sexass_WITNESSED,
                                     sexass_LEARNED,
                                     sexass_PART,
                                     sexass_UNSURE,
                                     sexass_DOESNTAPPLY,
                                     otherunw_HAPPENED,
                                     otherunw_WITNESSED,
                                     otherunw_LEARNED,
                                     otherunw_PART,
                                     otherunw_UNSURE,
                                     otherunw_DOESNTAPPLY,
                                     combat_HAPPENED,
                                     combat_WITNESSED,
                                     combat_LEARNED,
                                     combat_PART,
                                     combat_UNSURE,
                                     combat_DOESNTAPPLY,
                                     captiv_HAPPENED,
                                     captiv_WITNESSED,
                                     captiv_LEARNED,
                                     captiv_PART,
                                     captiv_UNSURE,
                                     captiv_DOESNTAPPLY,
                                     lifethreat_HAPPENED,
                                     lifethreat_WITNESSED,
                                     lifethreat_LEARNED,
                                     lifethreat_PART,
                                     lifethreat_UNSURE,
                                     lifethreat_DOESNTAPPLY,
                                     severehum_HAPPENED,
                                     severehum_WITNESSED,
                                     severehum_LEARNED,
                                     severehum_PART,
                                     severehum_UNSURE,
                                     severehum_DOESNTAPPLY,
                                     suddviol_HAPPENED,
                                     suddviol_WITNESSED,
                                     suddviol_LEARNED,
                                     suddviol_PART,
                                     suddviol_UNSURE,
                                     suddviol_DOESNTAPPLY,
                                     suddacci_HAPPENED,
                                     suddacci_WITNESSED,
                                     suddacci_LEARNED,
                                     suddacci_PART,
                                     suddacci_UNSURE,
                                     suddacci_DOESNTAPPLY,
                                     seriousinj_HAPPENED,
                                     seriousinj_WITNESSED,
                                     seriousinj_LEARNED,
                                     seriousinj_PART,
                                     seriousinj_UNSURE,
                                     seriousinj_DOESNTAPPLY,
                                     anyother_HAPPENED,
                                     anyother_WITNESSED,
                                     anyother_LEARNED,
                                     anyother_PART,
                                     anyother_UNSURE,
                                     anyother_DOESNTAPPLY
))




#________________________________________________________________________________________              
# Completeness check
#----------------------------------------------------------------------------------------

lec_score <- function(x)
{
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
  

 #checking for completeness
  
  variableshere <- c(natdis_HAPPENED,
                     natdis_WITNESSED,
                     natdis_LEARNED,
                     natdis_PART,
                     natdis_UNSURE,
                     natdis_DOESNTAPPLY,
                     fire_HAPPENED,
                     fire_WITNESSED,
                     fire_LEARNED,
                     fire_PART,
                     fire_UNSURE,
                     fire_DOESNTAPPLY,
                     accid_HAPPENED,
                     accid_WITNESSED,
                     accid_LEARNED,
                     accid_PART,
                     accid_UNSURE,
                     accid_DOESNTAPPLY,
                     seriousacc_HAPPENED,
                     seriousacc_WITNESSED,
                     seriousacc_LEARNED,
                     seriousacc_PART,
                     seriousacc_UNSURE,
                     seriousacc_DOESNTAPPLY,
                     expos_HAPPENED,
                     expos_WITNESSED,
                     expos_LEARNED,
                     expos_PART,
                     expos_UNSURE,
                     expos_DOESNTAPPLY,
                     physass_HAPPENED,
                     physass_WITNESSED,
                     physass_LEARNED,
                     physass_PART,
                     physass_UNSURE,
                     physass_DOESNTAPPLY,
                     assweap_HAPPENED,
                     assweap_WITNESSED,
                     assweap_LEARNED,
                     assweap_PART,
                     assweap_UNSURE,
                     assweap_DOESNTAPPLY,
                     sexass_HAPPENED,
                     sexass_WITNESSED,
                     sexass_LEARNED,
                     sexass_PART,
                     sexass_UNSURE,
                     sexass_DOESNTAPPLY,
                     otherunw_HAPPENED,
                     otherunw_WITNESSED,
                     otherunw_LEARNED,
                     otherunw_PART,
                     otherunw_UNSURE,
                     otherunw_DOESNTAPPLY,
                     combat_HAPPENED,
                     combat_WITNESSED,
                     combat_LEARNED,
                     combat_PART,
                     combat_UNSURE,
                     combat_DOESNTAPPLY,
                     captiv_HAPPENED,
                     captiv_WITNESSED,
                     captiv_LEARNED,
                     captiv_PART,
                     captiv_UNSURE,
                     captiv_DOESNTAPPLY,
                     lifethreat_HAPPENED,
                     lifethreat_WITNESSED,
                     lifethreat_LEARNED,
                     lifethreat_PART,
                     lifethreat_UNSURE,
                     lifethreat_DOESNTAPPLY,
                     severehum_HAPPENED,
                     severehum_WITNESSED,
                     severehum_LEARNED,
                     severehum_PART,
                     severehum_UNSURE,
                     severehum_DOESNTAPPLY,
                     suddviol_HAPPENED,
                     suddviol_WITNESSED,
                     suddviol_LEARNED,
                     suddviol_PART,
                     suddviol_UNSURE,
                     suddviol_DOESNTAPPLY,
                     suddacci_HAPPENED,
                     suddacci_WITNESSED,
                     suddacci_LEARNED,
                     suddacci_PART,
                     suddacci_UNSURE,
                     suddacci_DOESNTAPPLY,
                     seriousinj_HAPPENED,
                     seriousinj_WITNESSED,
                     seriousinj_LEARNED,
                     seriousinj_PART,
                     seriousinj_UNSURE,
                     seriousinj_DOESNTAPPLY,
                     anyother_HAPPENED,
                     anyother_WITNESSED,
                     anyother_LEARNED,
                     anyother_PART,
                     anyother_UNSURE,
                     anyother_DOESNTAPPLY)
  

  
data_complete_lec<- as.numeric(
  sum(
    is.na(
      c( variableshere 
      )
    )
  ) == 0
)

data_not_attempted_lec<- as.numeric(
  sum(
    is.na(
      c( variableshere 
      )
    )
  ) == 102
)

completeness_lec<- "1"
if(!(is.na(data_not_attempted_lec))){
  if(data_not_attempted_lec==1)
  {
    completeness_lec <- "not attempted"}else{}
}else{completeness_lec<-NA}

if(!(is.na(data_complete_lec))){
  if(data_complete_lec==1){
    completeness_lec <- "complete"} else{}
}else{completeness_lec<-NA}


if(data_not_attempted_lec==0 & data_complete_lec==0){
  completeness_lec <- "partially completed"}else{}

scores <- data.frame(data_complete_lec, data_not_attempted_lec, completeness_lec)

return(scores)
}


#Calculate summary scores in data 
datalec_scored <- adply(datleclife2, 1, lec_score)

attach(datalec_scored)

#Checking for criteria A
datalec_scored$CritA<- 
  datalec_scored$natdis_HAPPENED|
  datalec_scored$natdis_WITNESSED|
  datalec_scored$natdis_LEARNED|
  datalec_scored$natdis_PART|
  datalec_scored$fire_HAPPENED|
  datalec_scored$fire_WITNESSED|
  datalec_scored$fire_LEARNED|
  datalec_scored$fire_PART|
  datalec_scored$accid_HAPPENED|
  datalec_scored$accid_WITNESSED|
  datalec_scored$accid_LEARNED|
  datalec_scored$accid_PART|
  datalec_scored$seriousacc_HAPPENED|
  datalec_scored$seriousacc_WITNESSED|
  datalec_scored$seriousacc_LEARNED|
  datalec_scored$seriousacc_PART|
  datalec_scored$expos_HAPPENED|
  datalec_scored$expos_WITNESSED|
  datalec_scored$expos_LEARNED|
  datalec_scored$expos_PART|
  datalec_scored$physass_HAPPENED|
  datalec_scored$physass_WITNESSED|
  datalec_scored$physass_LEARNED|
  datalec_scored$physass_PART|
  datalec_scored$assweap_HAPPENED|
  datalec_scored$assweap_WITNESSED|
  datalec_scored$assweap_LEARNED|
  datalec_scored$assweap_PART|
  datalec_scored$sexass_HAPPENED|
  datalec_scored$sexass_WITNESSED|
  datalec_scored$sexass_LEARNED|
  datalec_scored$sexass_PART|
  datalec_scored$otherunw_HAPPENED|
  datalec_scored$otherunw_WITNESSED|
  datalec_scored$otherunw_LEARNED|
  datalec_scored$otherunw_PART|
  datalec_scored$combat_HAPPENED|
  datalec_scored$combat_WITNESSED|
  datalec_scored$combat_LEARNED|
  datalec_scored$combat_PART|
  datalec_scored$captiv_HAPPENED|
  datalec_scored$captiv_WITNESSED|
  datalec_scored$captiv_LEARNED|
  datalec_scored$captiv_PART|
  datalec_scored$lifethreat_HAPPENED|
  datalec_scored$lifethreat_WITNESSED|
  datalec_scored$lifethreat_LEARNED|
  datalec_scored$lifethreat_PART|
  datalec_scored$severehum_HAPPENED|
  datalec_scored$severehum_WITNESSED|
  datalec_scored$severehum_LEARNED|
  datalec_scored$severehum_PART|
  datalec_scored$suddviol_HAPPENED|
  datalec_scored$suddviol_WITNESSED|
  datalec_scored$suddviol_LEARNED|
  datalec_scored$suddviol_PART|
  datalec_scored$suddacci_HAPPENED|
  datalec_scored$suddacci_WITNESSED|
  datalec_scored$suddacci_LEARNED|
  datalec_scored$suddacci_PART|
  datalec_scored$seriousinj_HAPPENED|
  datalec_scored$seriousinj_WITNESSED|
  datalec_scored$seriousinj_LEARNED|
  datalec_scored$seriousinj_PART|
  datalec_scored$anyother_HAPPENED|
  datalec_scored$anyother_WITNESSED|
  datalec_scored$anyother_LEARNED|
  datalec_scored$anyother_PART

#varibales to check
list1<- 
  c("assessment_id", "natdis_HAPPENED",
  "natdis_WITNESSED",
  "natdis_LEARNED",
  "natdis_PART",
  "fire_HAPPENED",
  "fire_WITNESSED",
  "fire_LEARNED",
  "fire_PART",
  "accid_HAPPENED",
  "accid_WITNESSED",
  "accid_LEARNED",
  "accid_PART",
  "seriousacc_HAPPENED",
  "seriousacc_WITNESSED",
  "seriousacc_LEARNED",
  "seriousacc_PART",
  "expos_HAPPENED",
  "expos_WITNESSED",
  "expos_LEARNED",
  "expos_PART",
  "physass_HAPPENED",
  "physass_WITNESSED",
  "physass_LEARNED",
  "physass_PART",
  "assweap_HAPPENED",
  "assweap_WITNESSED",
  "assweap_LEARNED",
  "assweap_PART",
  "sexass_HAPPENED",
  "sexass_WITNESSED",
  "sexass_LEARNED",
  "sexass_PART",
  "otherunw_HAPPENED",
  "otherunw_WITNESSED",
  "otherunw_LEARNED",
  "otherunw_PART",
  "combat_HAPPENED",
  "combat_WITNESSED",
  "combat_LEARNED",
  "combat_PART",
  "captiv_HAPPENED",
  "captiv_WITNESSED",
  "captiv_LEARNED",
  "captiv_PART",
  "lifethreat_HAPPENED",
  "lifethreat_WITNESSED",
  "lifethreat_LEARNED",
  "lifethreat_PART",
  "severehum_HAPPENED",
  "severehum_WITNESSED",
  "severehum_LEARNED",
  "severehum_PART",
  "suddviol_HAPPENED",
  "suddviol_WITNESSED",
  "suddviol_LEARNED",
  "suddviol_PART",
  "suddacci_HAPPENED",
  "suddacci_WITNESSED",
  "suddacci_LEARNED",
  "suddacci_PART",
  "seriousinj_HAPPENED",
  "seriousinj_WITNESSED",
  "seriousinj_LEARNED",
  "seriousinj_PART",
  "anyother_HAPPENED",
  "anyother_WITNESSED",
  "anyother_LEARNED",
  "anyother_PART") 

  #check if any are a 1
  # rowSums("1" == list1) > 0
  
  #df$q1_smoke_drink = df$question1 | df$smoker | df$drinker
newdata <- datalec_scored[, list1]
datalec_scored$CritA2<-as.numeric(apply(newdata[2:69], 1, function(x) any(x == 1)))
  
detach(datalec_scored)

#to anonymize data
datalec_scored1<- within(datalec_scored,
                        {
                          assessment_id <- NULL
                          vista_lastname <- NULL
                        })

#________________________________________________________________________________________ 
#Export
#----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/17_LEC-5_lifetime/LEC-5_lifetime_UPDATED_scored_data_export.csv", sep="")
write.csv( datalec_scored  , filename,quote=T,row.names=F,na="#N/A")

filename <- paste("~/Biobank/17_LEC-5_lifetime/LEC-5_lifetime_UPDATED_scored_data_export_DEIDENTIFIED.csv", sep="")
write.csv( datalec_scored  , filename,quote=T,row.names=F,na="#N/A")
print("17_LEC_lifetime_NEW_done")


#return completness column
myvars <- c("assessment_id", "completeness_lec")
newdata <- datalec_scored[myvars]
return(newdata)
}

