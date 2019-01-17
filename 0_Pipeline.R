#########################################################################################
# Last Date modified: 01/17/2019
# Author: Katy Torres
# Description: Merge corefile with data export & run all scoring functions. 
#              Outputs will be stored in appropriaste questionnaire folders
##########################################################################################
library(tidyverse)
library(plyr)
#Replace this path with location where data is currently stored
setwd('C:/Users/Nievergelt Lab/Documents/Biobank/data')

#BEFORE READING IN DATA 
#  1. go in corefile and change visits variable to "visit_number" before merge. QC this.
#  2. look at "BB1051" sometimes it is incorrectly written as "Bb1051"
#  3. make sure it all makes sense
#  4. Add LEC names to the joined data export file

#________________________________________________________________________________________              
# READ IN DATA
# CHANGE FILE NAMES AND EXPORT DATE
#----------------------------------------------------------------------------------------
dataset <- read.csv('joined_data_export_2018_FREEZE_1.csv',header=T,na.strings=c("#N/A",NA))
core <- read.csv('biobank_data_corefile_2018_Freeze_1.csv',header=T, na.strings=c("",NA))
exportdate <- "2018_FREEZE_1"



#________________________________________________________________________________________  
# MERGE DATASETS TOGETHER
#------------------------------------------------------------------------
#merge CPRS corefile and full dataset by assesstment id % LAST NAME
dat00 <- merge(core, dataset, by=c("assessment_id", "vista_lastname"), all = TRUE)

#REMOVE EXCLUDED SUBJECTS
dat0 <- dat00 [ ! dat00$assessment_id %in% c(8835,17071), ]

#Export data
filename <- paste("~/Biobank/data/complete_database_", exportdate, ".csv", sep="")
write.csv(dat0, filename,quote=T, row.names=F,na="#N/A")



#________________________________________________________________________________________              
# RUN THROUGH PIPELINE
#----------------------------------------------------------------------------------------
setwd('C:/Users/Nievergelt Lab/Documents/Biobank/000_R_scripts')

#SOURCE IN SCRIPTS
#----------------------------------------------------------------------------------------
source("1_AUDIT_function.r")
# source("2_BAT_L_interval_function.r") #REMOVED
# source("3_BATL_function.r")           #REMOVED
source("4_BTBIS_function.r")
source("5_Basic_Demographic_function.r")
source("6_basic_pain_function.r")
source("7_Brief_CESAMH_function.r")
source("8_CDDR_function.r")
source("9_current_tx_function.r")
source("10_DRRI_CES_function.r")
source("11_DRRI_PBE_function.r")
source("12_Employment_function.r")
source("13_exposures_function.r")
source("14_GAD_function.r")
source("15_ISI_function.r")
source("16_ISI_MedQuestion_function.r")
source("17_LEC_5_lifetime_function.r")
source("17_LEC_5_lifetime_function_2_updated.r")
source("18_LEC5_PCL5_function.r")
source("19_MST2016_function_2.r") 
source("20_PCL5_lifetime_function.r")
source("21_PCL5_current_function.r") 
source("22_PHQ15_function.r")                    
source("23_PHQ9_function.r")      
source("24_PANAS_function.r")             
source("25_PROMIS_pain_intensity_function.r")        
source("26_Research_ID_function.r")               
source("27_service_history_function.r")            
source("28_demographic_social_function.r")        
source("29_tinnitus_screener_function_2.r")
source("30_treatment_history_function_2.r")        
source("31_WHODAS_function.r")    
source("32_HEQ_script_2.r") 
source("33_MASQ_script.r") 



#CALL EACH FUNCTION
#Calculate summary scores in data
#----------------------------------------------------------------------------------------
c1<- audit(dat0, exportdate)
#batl2(dat0, exportdate)       #NO LONGER IN USE
#batl3(dat0, exportdate)       #NO LONGER IN USE
c4<-btbis(dat0, exportdate)
basicdemo(dat0, exportdate)
basicpain(dat0, exportdate)
c7<- briefCESAM(dat0, exportdate)
c8<- cddr(dat0, exportdate)
#currtx(dat0, exportdate)      #NO LONGER IN USE
c10<-drrices(dat0, exportdate)
c11<-drripbe(dat0, exportdate)
c12<-employment(dat0, exportdate) 
c13<- exposures(dat0, exportdate) 
c14<- gad(dat0, exportdate)
c15<- isi(dat0, exportdate) 
c16<- isi2(dat0, exportdate) 
c171<- leclife(dat0, exportdate)     #OLD version of LEC
c172<- leclife2(dat0, exportdate)     #NEW version of LEC
c18<- lecpcl(dat0, exportdate) 
c19<- mst(dat0, exportdate)
c20<- pcllife(dat0, exportdate)
c21 <- pclcurrent(dat0, exportdate)
c22<- phq15(dat0, exportdate)               
c23<- phq9(dat0, exportdate) 
c24<- panas(dat0, exportdate)
c25<-promis(dat0, exportdate)
rid(dat0, exportdate)
c27<- service(dat0, exportdate)
c28<- social(dat0, exportdate)
c29<-tinnitus(dat0, exportdate)
c30<- treathist(dat0, exportdate)           
c31<- whodas(dat0, exportdate)
c32<- HEQfunc(dat0, exportdate) 
c33<- masq(dat0, exportdate)

c171a <- na.omit(c171)
c172a <- na.omit(c172)
#combine old and new LEC
lecboth <- merge(c171a, c172a, by=c("assessment_id"), all = FALSE)
#check both old and new LEC completeness
for(i in 1:nrow(lecboth)){
  if(lecboth$completeness_lec1[i]=="not attempted" & lecboth$completeness_lec[i]=="not attempted")
    {lecboth$completeness_lec_both[i]<- "not attempted"
  }else if(lecboth$completeness_lec1[i]=="complete" || lecboth$completeness_lec[i]=="complete")
  {lecboth$completeness_lec_both[i]<- "complete"
  }else{lecboth$completeness_lec_both[i]<- "partially completed"}
}

c17<- lecboth[,-2:-3]

completelist<- list(c1,c4,c7,c8,c10,c11,c12,c13, c14, c15, c16, c17, c18, c19, c20, c21, c22, c23, c24, c27, c28, c29, c30, c31,c32,c33)
#completedataframe<-completelist %>% reduce(left_join, by = "assessment_id")
completedataframe<-join_all(completelist, by="assessment_id")
View(completedataframe)

completedataframe1<- merge(core, completedataframe, by=c("assessment_id"), all = TRUE)

#Export data
filename <- paste("~/Biobank/data/completeness_table_", exportdate, ".csv", sep="")
write.csv(completedataframe1, filename,quote=T, row.names=F,na="#N/A")