#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 17, LEC-5 (lifetime)
##########################################################################################
leclife<- function(dat0, exportdate)
{
#Load plyr library
library(plyr)

#Only retain relevant variables
datleclife <- subset(dat0, select= c(assessment_id,vista_lastname, visit_number,
                              LEC_5_1_natdis,
                              LEC_5_2_fire,
                              LEC_5_3_accid,
                              
                              LEC_5_4_seriousacc,
                              LEC_5_5_expos,
                              LEC_5_6_physass,
                              LEC_5_7_assweap,
                              LEC_5_8_sexass,
                              
                              LEC_5_9_otherunw,
                              LEC_5_10_combat,
                              LEC_5_11_captiv,
                              LEC_5_12_life.threat,
                              LEC_5_13_severehum,
                              
                              LEC_5_14_suddviol,
                              LEC_5_15_suddacci,
                              LEC_5_16_seriousinj,
                              LEC_5_17_anyother))

#if any LEC scored 1-4 LEC, then Criteria A is yes

df<- datleclife[,4:19]
crita1<- rowSums(df == 1 | df == 2 |df ==3 | df==4, na.rm = TRUE) > 0L
datleclife$critA1<- as.numeric(crita1)
#________________________________________________________________________________________ 
#Export
#----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/17_LEC-5_lifetime/LEC-5_lifetime_reduced_data_export_", exportdate, ".csv", sep="")
write.csv( datleclife, filename,quote=T,row.names=F,na="#N/A")

return(print("17_LEC_lifetime_done"))
}

