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
                                      LEC5_1_nat_htm,
                                      LEC5_1_nat_wi,
                                      LEC5_1_nat_lai,
                                      LEC5_1_nat_pomj,
                                      LEC5_1_nat_ns,
                                      LEC5_1_nat_da,
                                      LEC5_2_fir_htm,
                                      LEC5_2_fir_wi,
                                      LEC5_2_fir_lai,
                                      LEC5_2_fir_pomj,
                                      LEC5_2_fir_ns,
                                      LEC5_2_fir_da,
                                      LEC5_3_acc_htm,
                                      LEC5_3_acc_wi,
                                      LEC5_3_acc_lai,
                                      LEC5_3_acc_pomj,
                                      LEC5_3_acc_ns,
                                      LEC5_3_acc_da,
                                      LEC5_4_ser_htm,
                                      LEC5_4_ser_wi,
                                      LEC5_4_ser_lai,
                                      LEC5_4_ser_pomj,
                                      LEC5_4_ser_ns,
                                      LEC5_4_ser_da,
                                      LEC5_5_exp_htm,
                                      LEC5_5_exp_wi,
                                      LEC5_5_exp_lai,
                                      LEC5_5_exp_pomj,
                                      LEC5_5_exp_ns,
                                      LEC5_5_exp_da,
                                      LEC5_6_phy_htm,
                                      LEC5_6_phy_wi,
                                      LEC5_6_phy_lai,
                                      LEC5_6_phy_pomj,
                                      LEC5_6_phy_ns,
                                      LEC5_6_phy_da,
                                      LEC5_7_ass_htm,
                                      LEC5_7_ass_wi,
                                      LEC5_7_ass_lai,
                                      LEC5_7_ass_pomj,
                                      LEC5_7_ass_ns,
                                      LEC5_7_ass_da,
                                      LEC5_8_sex_htm,
                                      LEC5_8_sex_wi,
                                      LEC5_8_sex_lai,
                                      LEC5_8_sex_pomj,
                                      LEC5_8_sex_ns,
                                      LEC5_8_sex_da,
                                      LEC5_9_oth_htm,
                                      LEC5_9_oth_wi,
                                      LEC5_9_oth_lai,
                                      LEC5_9_oth_pomj,
                                      LEC5_9_oth_ns,
                                      LEC5_9_oth_da,
                                      LEC5_10_com_htm,
                                      LEC5_10_com_wi,
                                      LEC5_10_com_lai,
                                      LEC5_10_com_pomj,
                                      LEC5_10_com_ns,
                                      LEC5_10_com_da,
                                      LEC5_11_cap_htm,
                                      LEC5_11_cap_wi,
                                      LEC5_11_cap_lai,
                                      LEC5_11_cap_pomj,
                                      LEC5_11_cap_ns,
                                      LEC5_11_cap_da,
                                      LEC5_12_lif_htm,
                                      LEC5_12_lif_wi,
                                      LEC5_12_lif_lai,
                                      LEC5_12_lif_pomj,
                                      LEC5_12_lif_ns,
                                      LEC5_12_lif_da,
                                      LEC5_13_sev_htm,
                                      LEC5_13_sev_wi,
                                      LEC5_13_sev_lai,
                                      LEC5_13_sev_pomj,
                                      LEC5_13_sev_ns,
                                      LEC5_13_sev_da,
                                      LEC5_14_sudv_htm,
                                      LEC5_14_sudv_wi,
                                      LEC5_14_sudv_lai,
                                      LEC5_14_sudv_pomj,
                                      LEC5_14_sudv_ns,
                                      LEC5_14_sudv_da,
                                      LEC5_15_suda_htm,
                                      LEC5_15_suda_wi,
                                      LEC5_15_suda_lai,
                                      LEC5_15_suda_pomj,
                                      LEC5_15_suda_ns,
                                      LEC5_15_suda_da,
                                      LEC5_16_ser_htm,
                                      LEC5_16_ser_wi,
                                      LEC5_16_ser_lai,
                                      LEC5_16_ser_pomj,
                                      LEC5_16_ser_ns,
                                      LEC5_16_ser_da,
                                      LEC5_17_any_htm,
                                      LEC5_17_any_wi,
                                      LEC5_17_any_lai,
                                      LEC5_17_any_pomj,
                                      LEC5_17_any_ns,
                                      LEC5_17_any_da,
                                      LEC5_17a_other
))




#________________________________________________________________________________________              
# Completeness check
#----------------------------------------------------------------------------------------

lec_score <- function(x)
{
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
  

 #checking for completeness
  
  variableshere <- c(LEC5_1_nat_htm,
                     LEC5_1_nat_wi,
                     LEC5_1_nat_lai,
                     LEC5_1_nat_pomj,
                     LEC5_1_nat_ns,
                     LEC5_1_nat_da,
                     LEC5_2_fir_htm,
                     LEC5_2_fir_wi,
                     LEC5_2_fir_lai,
                     LEC5_2_fir_pomj,
                     LEC5_2_fir_ns,
                     LEC5_2_fir_da,
                     LEC5_3_acc_htm,
                     LEC5_3_acc_wi,
                     LEC5_3_acc_lai,
                     LEC5_3_acc_pomj,
                     LEC5_3_acc_ns,
                     LEC5_3_acc_da,
                     LEC5_4_ser_htm,
                     LEC5_4_ser_wi,
                     LEC5_4_ser_lai,
                     LEC5_4_ser_pomj,
                     LEC5_4_ser_ns,
                     LEC5_4_ser_da,
                     LEC5_5_exp_htm,
                     LEC5_5_exp_wi,
                     LEC5_5_exp_lai,
                     LEC5_5_exp_pomj,
                     LEC5_5_exp_ns,
                     LEC5_5_exp_da,
                     LEC5_6_phy_htm,
                     LEC5_6_phy_wi,
                     LEC5_6_phy_lai,
                     LEC5_6_phy_pomj,
                     LEC5_6_phy_ns,
                     LEC5_6_phy_da,
                     LEC5_7_ass_htm,
                     LEC5_7_ass_wi,
                     LEC5_7_ass_lai,
                     LEC5_7_ass_pomj,
                     LEC5_7_ass_ns,
                     LEC5_7_ass_da,
                     LEC5_8_sex_htm,
                     LEC5_8_sex_wi,
                     LEC5_8_sex_lai,
                     LEC5_8_sex_pomj,
                     LEC5_8_sex_ns,
                     LEC5_8_sex_da,
                     LEC5_9_oth_htm,
                     LEC5_9_oth_wi,
                     LEC5_9_oth_lai,
                     LEC5_9_oth_pomj,
                     LEC5_9_oth_ns,
                     LEC5_9_oth_da,
                     LEC5_10_com_htm,
                     LEC5_10_com_wi,
                     LEC5_10_com_lai,
                     LEC5_10_com_pomj,
                     LEC5_10_com_ns,
                     LEC5_10_com_da,
                     LEC5_11_cap_htm,
                     LEC5_11_cap_wi,
                     LEC5_11_cap_lai,
                     LEC5_11_cap_pomj,
                     LEC5_11_cap_ns,
                     LEC5_11_cap_da,
                     LEC5_12_lif_htm,
                     LEC5_12_lif_wi,
                     LEC5_12_lif_lai,
                     LEC5_12_lif_pomj,
                     LEC5_12_lif_ns,
                     LEC5_12_lif_da,
                     LEC5_13_sev_htm,
                     LEC5_13_sev_wi,
                     LEC5_13_sev_lai,
                     LEC5_13_sev_pomj,
                     LEC5_13_sev_ns,
                     LEC5_13_sev_da,
                     LEC5_14_sudv_htm,
                     LEC5_14_sudv_wi,
                     LEC5_14_sudv_lai,
                     LEC5_14_sudv_pomj,
                     LEC5_14_sudv_ns,
                     LEC5_14_sudv_da,
                     LEC5_15_suda_htm,
                     LEC5_15_suda_wi,
                     LEC5_15_suda_lai,
                     LEC5_15_suda_pomj,
                     LEC5_15_suda_ns,
                     LEC5_15_suda_da,
                     LEC5_16_ser_htm,
                     LEC5_16_ser_wi,
                     LEC5_16_ser_lai,
                     LEC5_16_ser_pomj,
                     LEC5_16_ser_ns,
                     LEC5_16_ser_da,
                     LEC5_17_any_htm,
                     LEC5_17_any_wi,
                     LEC5_17_any_lai,
                     LEC5_17_any_pomj,
                     LEC5_17_any_ns,
                     LEC5_17_any_da)
  

  
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
  datalec_scored$LEC5_1_nat_htm|
  datalec_scored$LEC5_1_nat_wi|
  datalec_scored$LEC5_1_nat_lai|
  datalec_scored$LEC5_1_nat_pomj|
  datalec_scored$LEC5_2_fir_htm|
  datalec_scored$LEC5_2_fir_wi|
  datalec_scored$LEC5_2_fir_lai|
  datalec_scored$LEC5_2_fir_pomj|
  datalec_scored$LEC5_3_acc_htm|
  datalec_scored$LEC5_3_acc_wi|
  datalec_scored$LEC5_3_acc_lai|
  datalec_scored$LEC5_3_acc_pomj|
  datalec_scored$LEC5_4_ser_htm|
  datalec_scored$LEC5_4_ser_wi|
  datalec_scored$LEC5_4_ser_lai|
  datalec_scored$LEC5_4_ser_pomj|
  datalec_scored$LEC5_5_exp_htm|
  datalec_scored$LEC5_5_exp_wi|
  datalec_scored$LEC5_5_exp_lai|
  datalec_scored$LEC5_5_exp_pomj|
  datalec_scored$LEC5_6_phy_htm|
  datalec_scored$LEC5_6_phy_wi|
  datalec_scored$LEC5_6_phy_lai|
  datalec_scored$LEC5_6_phy_pomj|
  datalec_scored$LEC5_7_ass_htm|
  datalec_scored$LEC5_7_ass_wi|
  datalec_scored$LEC5_7_ass_lai|
  datalec_scored$LEC5_7_ass_pomj|
  datalec_scored$LEC5_8_sex_htm|
  datalec_scored$LEC5_8_sex_wi|
  datalec_scored$LEC5_8_sex_lai|
  datalec_scored$LEC5_8_sex_pomj|
  datalec_scored$LEC5_9_oth_htm|
  datalec_scored$LEC5_9_oth_wi|
  datalec_scored$LEC5_9_oth_lai|
  datalec_scored$LEC5_9_oth_pomj|
  datalec_scored$LEC5_10_com_htm|
  datalec_scored$LEC5_10_com_wi|
  datalec_scored$LEC5_10_com_lai|
  datalec_scored$LEC5_10_com_pomj|
  datalec_scored$LEC5_11_cap_htm|
  datalec_scored$LEC5_11_cap_wi|
  datalec_scored$LEC5_11_cap_lai|
  datalec_scored$LEC5_11_cap_pomj|
  datalec_scored$LEC5_12_lif_htm|
  datalec_scored$LEC5_12_lif_wi|
  datalec_scored$LEC5_12_lif_lai|
  datalec_scored$LEC5_12_lif_pomj|
  datalec_scored$LEC5_13_sev_htm|
  datalec_scored$LEC5_13_sev_wi|
  datalec_scored$LEC5_13_sev_lai|
  datalec_scored$LEC5_13_sev_pomj|
  datalec_scored$LEC5_14_sudv_htm|
  datalec_scored$LEC5_14_sudv_wi|
  datalec_scored$LEC5_14_sudv_lai|
  datalec_scored$LEC5_14_sudv_pomj|
  datalec_scored$LEC5_15_suda_htm|
  datalec_scored$LEC5_15_suda_wi|
  datalec_scored$LEC5_15_suda_lai|
  datalec_scored$LEC5_15_suda_pomj|
  datalec_scored$LEC5_16_ser_htm|
  datalec_scored$LEC5_16_ser_wi|
  datalec_scored$LEC5_16_ser_lai|
  datalec_scored$LEC5_16_ser_pomj|
  datalec_scored$LEC5_17_any_htm|
  datalec_scored$LEC5_17_any_wi|
  datalec_scored$LEC5_17_any_lai|
  datalec_scored$LEC5_17_any_pomj
  

#varibales to check
list1<- 
  c("assessment_id",
    "LEC5_1_nat_htm",
    "LEC5_1_nat_wi",
    "LEC5_1_nat_lai",
    "LEC5_1_nat_pomj",
    "LEC5_2_fir_htm",
    "LEC5_2_fir_wi",
    "LEC5_2_fir_lai",
    "LEC5_2_fir_pomj",
    "LEC5_3_acc_htm",
    "LEC5_3_acc_wi",
    "LEC5_3_acc_lai",
    "LEC5_3_acc_pomj",
    "LEC5_4_ser_htm",
    "LEC5_4_ser_wi",
    "LEC5_4_ser_lai",
    "LEC5_4_ser_pomj",
    "LEC5_5_exp_htm",
    "LEC5_5_exp_wi",
    "LEC5_5_exp_lai",
    "LEC5_5_exp_pomj",
    "LEC5_6_phy_htm",
    "LEC5_6_phy_wi",
    "LEC5_6_phy_lai",
    "LEC5_6_phy_pomj",
    "LEC5_7_ass_htm",
    "LEC5_7_ass_wi",
    "LEC5_7_ass_lai",
    "LEC5_7_ass_pomj",
    "LEC5_8_sex_htm",
    "LEC5_8_sex_wi",
    "LEC5_8_sex_lai",
    "LEC5_8_sex_pomj",
    "LEC5_9_oth_htm",
    "LEC5_9_oth_wi",
    "LEC5_9_oth_lai",
    "LEC5_9_oth_pomj",
    "LEC5_10_com_htm",
    "LEC5_10_com_wi",
    "LEC5_10_com_lai",
    "LEC5_10_com_pomj",
    "LEC5_11_cap_htm",
    "LEC5_11_cap_wi",
    "LEC5_11_cap_lai",
    "LEC5_11_cap_pomj",
    "LEC5_12_lif_htm",
    "LEC5_12_lif_wi",
    "LEC5_12_lif_lai",
    "LEC5_12_lif_pomj",
    "LEC5_13_sev_htm",
    "LEC5_13_sev_wi",
    "LEC5_13_sev_lai",
    "LEC5_13_sev_pomj",
    "LEC5_14_sudv_htm",
    "LEC5_14_sudv_wi",
    "LEC5_14_sudv_lai",
    "LEC5_14_sudv_pomj",
    "LEC5_15_suda_htm",
    "LEC5_15_suda_wi",
    "LEC5_15_suda_lai",
    "LEC5_15_suda_pomj",
    "LEC5_16_ser_htm",
    "LEC5_16_ser_wi",
    "LEC5_16_ser_lai",
    "LEC5_16_ser_pomj",
    "LEC5_17_any_htm",
    "LEC5_17_any_wi",
    "LEC5_17_any_lai",
    "LEC5_17_any_pomj")

  #check if any are a 1
  # rowSums("1" == list1) > 0
  
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
# 
# filename <- paste("~/Biobank/17_LEC-5_lifetime/LEC-5_lifetime_UPDATED_scored_data_export_DEIDENTIFIED.csv", sep="")
# write.csv( datalec_scored  , filename,quote=T,row.names=F,na="#N/A")
print("17_LEC_lifetime_NEW_done")


#return completness column
myvars <- c("assessment_id", "completeness_lec")
newdata <- datalec_scored[myvars]
return(newdata)
}

