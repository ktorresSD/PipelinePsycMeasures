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


#________________________________________________________________________________________              
# FLAG
#----------------------------------------------------------------------------------------
#Make Flag for people that said "DOESN'T APPLY" and also" yes to "Happened, witnesssed, etc)
#Count how many times this flag occurs by person (Max possible 17)

#only do this code for the people following assessment 12144. 12144 and before could not have multiple responses
# flagsubset <- datalec_scored[ which(datalec_scored$assessment_id > 12144), ]
# 

flag_func <- function(x)
{
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
  
  
  if(assessment_id > 12144){
    if(!(is.na(LEC5_1_nat_da | LEC5_1_nat_htm | LEC5_1_nat_wi |LEC5_1_nat_lai | LEC5_1_nat_pomj))){
      if(LEC5_1_nat_da== 1 & (LEC5_1_nat_htm==1 | LEC5_1_nat_wi==1 |LEC5_1_nat_lai==1 | LEC5_1_nat_pomj==1)){flag1 <- 1}else{flag1<- 0}
    }else{flag1<-NA}
    
    if(!(is.na(LEC5_2_fir_da))){
      if(LEC5_2_fir_da== 1 & (LEC5_2_fir_htm==1 | LEC5_2_fir_wi==1 |LEC5_2_fir_lai==1 | LEC5_2_fir_pomj==1)){flag2 <- 1}else{flag2<- 0}
    }else{flag2<NA}
    
    if(!(is.na(LEC5_3_acc_da))){
      if(LEC5_3_acc_da== 1 & (LEC5_3_acc_htm==1 | LEC5_3_acc_wi==1 |LEC5_3_acc_lai==1 | LEC5_3_acc_pomj==1)){ flag3 <- 1}else{flag3<- 0}
    }else{flag3<-NA}
    
    if(!(is.na(LEC5_4_ser_da))){
      if(LEC5_4_ser_da== 1 & (LEC5_4_ser_htm==1 | LEC5_4_ser_wi==1 |LEC5_4_ser_lai==1 | LEC5_4_ser_pomj==1)){ flag4 <- 1}else{flag4<- 0}
    }else{flag4<-NA}
    
    if(!(is.na(LEC5_5_exp_da))){
      if(LEC5_5_exp_da== 1 & (LEC5_5_exp_htm==1 | LEC5_5_exp_wi==1 |LEC5_5_exp_lai==1 | LEC5_5_exp_pomj==1)){ flag5 <- 1}else{ flag5<- 0}
    }else{ flag5<-NA}
    
    if(!(is.na(LEC5_6_phy_da))){
      if(LEC5_6_phy_da== 1 & (LEC5_6_phy_htm==1 | LEC5_6_phy_wi==1 |LEC5_6_phy_lai==1 | LEC5_6_phy_pomj==1)){ flag6 <- 1}else{ flag6<- 0}
    }else{flag6<-NA}
    
    if(!(is.na(LEC5_7_ass_da))){
      if(LEC5_7_ass_da== 1 & (LEC5_7_ass_htm==1 | LEC5_7_ass_wi==1 |LEC5_7_ass_lai==1 | LEC5_7_ass_pomj==1)){ flag7 <- 1}else{ flag7<- 0}
    }else{flag7<-NA}
    
    if(!(is.na(LEC5_8_sex_da))){
      if(LEC5_8_sex_da== 1 & (LEC5_8_sex_htm==1 | LEC5_8_sex_wi==1 |LEC5_8_sex_lai==1 | LEC5_8_sex_pomj==1)){ flag8 <- 1}else{ flag8<- 0}
    }else{flag8<-NA}
    
    if(!(is.na(LEC5_9_oth_da))){
      if(LEC5_9_oth_da== 1 & (LEC5_9_oth_htm==1 | LEC5_9_oth_wi==1 |LEC5_9_oth_lai==1 | LEC5_9_oth_pomj==1)){ flag9 <- 1}else{ flag9<- 0}
    }else{flag9<-NA}
    
    if(!(is.na(LEC5_10_com_da))){
      if(LEC5_10_com_da== 1 & (LEC5_10_com_htm==1 | LEC5_10_com_wi==1 |LEC5_10_com_lai==1 | LEC5_10_com_pomj==1)){ flag10 <- 1}else{ flag10<- 0}
    }else{flag10<-NA}
    
    if(!(is.na(LEC5_11_cap_da))){
      if(LEC5_11_cap_da== 1 & (LEC5_11_cap_htm==1 | LEC5_11_cap_wi==1 |LEC5_11_cap_lai==1 | LEC5_11_cap_pomj==1)){ flag11 <- 1}else{ flag11<- 0}
    }else{flag11<-NA}
    
    if(!(is.na(LEC5_12_lif_da))){
      if(LEC5_12_lif_da== 1 & (LEC5_12_lif_htm==1 | LEC5_12_lif_wi==1 |LEC5_12_lif_lai==1 | LEC5_12_lif_pomj==1)){ flag12 <- 1}else{ flag12<- 0}
    }else{flag12<-NA}
    
    if(!(is.na(LEC5_13_sev_da))){
      if(LEC5_13_sev_da== 1 & (LEC5_13_sev_htm==1 | LEC5_13_sev_wi==1 |LEC5_13_sev_lai==1 | LEC5_13_sev_pomj==1)){ flag13 <- 1}else{ flag13<- 0}
    }else{flag13<-NA}
    
    if(!(is.na(LEC5_14_sudv_da))){
      if(LEC5_14_sudv_da== 1 & (LEC5_14_sudv_htm==1 | LEC5_14_sudv_wi==1 |LEC5_14_sudv_lai==1 | LEC5_14_sudv_pomj==1)){ flag14 <- 1}else{ flag14<- 0}
    }else{flag14<-NA}
    
    if(!(is.na(LEC5_15_suda_da))){
      if(LEC5_15_suda_da== 1 & (LEC5_15_suda_htm==1 | LEC5_15_suda_wi==1 |LEC5_15_suda_lai==1 | LEC5_15_suda_pomj==1)){ flag15 <- 1}else{ flag15<- 0}
    }else{flag15<-NA}
    
    if(!(is.na(LEC5_16_ser_da))){
      if(LEC5_16_ser_da== 1 & (LEC5_16_ser_htm==1 | LEC5_16_ser_wi==1 |LEC5_16_ser_lai==1 | LEC5_16_ser_pomj==1)){ flag16 <- 1}else{ flag16<- 0}
    }else{flag16<-NA}
    
    if(!(is.na(LEC5_17_any_da))){
      if(LEC5_17_any_da== 1 & (LEC5_17_any_htm==1 | LEC5_17_any_wi==1 |LEC5_17_any_lai==1 | LEC5_17_any_pomj==1)){ flag17 <- 1}else{ flag17<- 0}
    }else{flag17<-NA}
    
    
  }else if(assessment_id <= 12144){
    flag1<-0
    flag2<-0
    flag3<-0
    flag4<-0
    flag5<-0
    flag6<-0
    flag7<-0
    flag8<-0
    flag9<-0
    flag10<-0
    flag11<-0
    flag12<-0
    flag13<-0
    flag14<-0
    flag15<-0
    flag16<-0
    flag17<-0
  }
  
  happened_to_me_sum <- sum(c(LEC5_1_nat_htm,
                              LEC5_2_fir_htm,
                              LEC5_3_acc_htm,
                              LEC5_4_ser_htm,
                              LEC5_5_exp_htm,
                              LEC5_6_phy_htm,
                              LEC5_7_ass_htm,
                              LEC5_8_sex_htm,
                              LEC5_9_oth_htm,
                              LEC5_10_com_htm,
                              LEC5_11_cap_htm,
                              LEC5_12_lif_htm,
                              LEC5_13_sev_htm,
                              LEC5_14_sudv_htm,
                              LEC5_15_suda_htm,
                              LEC5_16_ser_htm,
                              LEC5_17_any_htm), na.rm = TRUE)
  
  
  flags_total <- flag1 + flag2 + flag3 + flag4 + flag5 + flag6 + 
    flag7 + flag8 + flag9 + flag10 + flag11 + flag12 + 
    flag13 + flag14 + flag15 + flag16 + flag17
  
  flags_total_incomplete <- sum(c(flag1 , flag2 , flag3 , flag4 , flag5 , flag6 , 
    flag7 , flag8 , flag9 , flag10 , flag11 , flag12 , 
    flag13 , flag14 , flag15 , flag16 , flag17), na.rm = TRUE)
  
  scores <- data.frame(happened_to_me_sum, flags_total, flags_total_incomplete  )
  
  return(scores)
}


#Calculate summary scores in data 
datalec_scored <- adply(datalec_scored, 1, flag_func)

# if(datalec_scored$assessment_id <= 12144){
#   flag1<-0
#   flag2<-0
#   flag3<-0
#   flag4<-0
#   flag5<-0
#   flag6<-0
# }



#to anonymize data
datalec_scored1<- within(datalec_scored,
                        {
                          assessment_id <- NULL
                          vista_lastname <- NULL
                        })


#________________________________________________________________________________________              
# Descriptive Stats and plots
#----------------------------------------------------------------------------------------
#subset by visit to get report information
v1 <- datalec_scored[ which(datalec_scored$visit_number==1), ]
v2 <- datalec_scored[ which(datalec_scored$visit_number==2), ]
v3 <- datalec_scored[ which(datalec_scored$visit_number==3), ]

#completeness table
table(datalec_scored$completeness_lec, datalec_scored$visit_number)


datalec_scored$CritA2


#________________________________________________________________________________________ 
#Export
#----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/17_LEC-5_lifetime/LEC-5_lifetime_UPDATED_scored_data_export.csv", sep="")
write.csv(datalec_scored, filename,quote=T,row.names=F,na="NA")
# 
filename <- paste("~/Biobank/17_LEC-5_lifetime/LEC-5_lifetime_UPDATED_scored_data_export_DEIDENTIFIED.csv", sep="")
write.csv( datalec_scored1 , filename,quote=T,row.names=F,na="NA")
print("17_LEC_lifetime_NEW_done")


#return completness column
myvars <- c("assessment_id", "completeness_lec")
newdata <- datalec_scored[myvars]
return(newdata)
}

