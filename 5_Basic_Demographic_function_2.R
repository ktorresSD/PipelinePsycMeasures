#########################################################################################
# Last Date modified: 11/05/2018
# Author: Katy Torres
# Description: Subset of question 5, Basic Demographic and scoring functions
##########################################################################################
basicdemo<- function(dat0, exportdate)
{

#Only retain relevant variables
datdemo1 <- subset(dat0, 
              select= c(assessment_id,vista_lastname,visit_number,
                        date_created,
                        demo_gender_r,
                        demo_YOB_r,
                        demo_weight_r,
                        demo_heightft_r,
                        demo_heightinch_r,
                        demo_ethnic_r,
                        
                        demo_racewhite,
                        demo_race_black,
                        demo_race_amind,
                        demo_race_pacisl,
                        demo_race_asian,
                        demo_race_decline,
                        demo_race_oth,
                        demo_race_oth_spec,
                
                        demo_relationship_r
                        
              ))


datdemo1$year_assessed<- format(as.Date(datdemo1$date_created, format="%m/%d/%Y"),"%Y")
datdemo1$approx_age<- as.numeric(datdemo1$year_assessed) - datdemo1$demo_YOB_r



#________________________________________________________________________________________              
# SCORING Functions Defined
#----------------------------------------------------------------------------------------
#Scoring function defined
demorace<- function(x)
{
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
  
  
  
  #One column for Diagnosis based on score
  
  if(!(is.na(demo_racewhite))){
    if(demo_racewhite==1)
    {
      race <- "White"}else{}
  }else{race <- NA}
  
  if(!(is.na(demo_race_black))){
    if(demo_race_black==1){
      race <- "Black"} else{}
  }else{race <-NA}
  
  if(!(is.na(demo_race_amind))){
    if(demo_race_amind==1){
      race <- "American Indian"}else{}
  }else{race<-NA}
  
  if(!(is.na(demo_race_pacisl))){
    if(demo_race_pacisl==1){
      race <- "Pacific Islander"}else{}
  }else{race <-NA}
  
  if(!(is.na(demo_race_asian))){
    if(demo_race_asian==1){
      race <- "Asian"}else{}
  }else{race <-NA}
  
  if(!(is.na(demo_race_decline))){
    if(demo_race_decline==1){
      race <- "Decline To Answer"}else{}
  }else{race <-NA}
  
  if(!(is.na(demo_race_oth))){
    if(demo_race_oth==1){
      race <- "Other"}else{}
  }else{race <-NA}
  
  if(!(is.na(demo_racewhite |demo_race_black |demo_race_amind|demo_race_pacisl|demo_race_asian|demo_race_oth))){
    if(sum(c(demo_racewhite, demo_race_black, demo_race_amind, demo_race_pacisl, demo_race_asian, demo_race_oth)) > 1 ){
      race <- "Multiple"} else{}
  }else{race <-NA}
  
  
  #completeness checks
  
  race_complete <- as.numeric(
    sum(
      is.na(
        c(demo_racewhite,
          demo_race_black,
          demo_race_amind,
          demo_race_pacisl,
          demo_race_asian,
          demo_race_decline,
          demo_race_oth))) == 0)
  
  race_not_attempted<- as.numeric(
    sum(
      is.na(
        c(demo_racewhite,
          demo_race_black,
          demo_race_amind,
          demo_race_pacisl,
          demo_race_asian,
          demo_race_decline,
          demo_race_oth))) == 7)
  
  completeness_race<- "1"
  if(!(is.na(race_not_attempted))){
    if(race_not_attempted==1)
    {
      completeness_race <- "not attempted"}else{}
  }else{completeness_race<-NA}
  
  if(!(is.na(race_complete))){
    if(race_complete==1){
      completeness_race <- "complete"} else{}
  }else{completeness_race<-NA}
  
  if(race_not_attempted==0 & race_complete==0){
    completeness_race <- "partially completed"}else{}
  
  scoresisi <- data.frame(completeness_race, race)
  
  return(scoresisi)
}


#Calculate summary scores in data
datdemorace <- adply(datdemo1, 1, demorace)


democomplete<- function(x)
{
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
  
  #completeness checks
  
  demo_complete <- as.numeric(
    sum(
      is.na(
        c(demo_gender_r,
          demo_YOB_r,
          demo_weight_r,
          demo_heightft_r,
          demo_heightinch_r,
          demo_ethnic_r,
          demo_racewhite,
          demo_race_black,
          demo_race_amind,
          demo_race_pacisl,
          demo_race_asian,
          demo_race_decline,
          demo_race_oth,
          demo_relationship_r))) == 0)
  
  demo_not_attempted<- as.numeric(
    sum(
      is.na(
        c(demo_gender_r,
          demo_YOB_r,
          demo_weight_r,
          demo_heightft_r,
          demo_heightinch_r,
          demo_ethnic_r,
          demo_racewhite,
          demo_race_black,
          demo_race_amind,
          demo_race_pacisl,
          demo_race_asian,
          demo_race_decline,
          demo_race_oth,
          demo_relationship_r))) == 14)
  
  completeness_demo<- "1"
  if(!(is.na(demo_not_attempted))){
    if(demo_not_attempted==1)
    {
      completeness_demo <- "not attempted"}else{}
  }else{completeness_demo<-NA}
  
  if(!(is.na(demo_complete))){
    if(demo_complete==1){
      completeness_demo <- "complete"} else{}
  }else{completeness_demo<-NA}
  
  if(demo_not_attempted==0 & demo_complete==0){
    completeness_demo <- "partially completed"}else{}
  
  scoresdemo_comp <- data.frame(completeness_demo)
  
  return(scoresdemo_comp)
}


#Calculate summary scores in data
datdemo_final <- adply(datdemorace, 1, democomplete)


#to anonymize data
datdemo_final1<- within(datdemo_final,
                  {
                    assessment_id <- NULL
                    vista_lastname <- NULL
                  })

#________________________________________________________________________________________              
# Checking consistency
#----------------------------------------------------------------------------------------
#attach(datdemo_final)

datdemo_final$sum_multiple <- sum(c(demo_racewhite, demo_race_black, demo_race_amind, demo_race_pacisl, demo_race_asian, 
demo_race_decline, demo_race_oth),na.rm=T)

#________________________________________________________________________________________              
# Descriptive Stats and plots
#----------------------------------------------------------------------------------------

#subset by visit to get report information
v1 <- datdemo_final[ which(datdemo_final$visit_number==1), ]
v2 <- datdemo_final[ which(datdemo_final$visit_number==2), ]
v3 <- datdemo_final[ which(datdemo_final$visit_number==3), ]

#completeness table
table(datdemo_final$completeness_demo, datdemo_final$visit_number)

table(datdemo_final$demo_gender_r)

#------------------------------------------------------------------------
#DATA TRANSFORMATION to see concordance between visits 
#------------------------------------------------------------------------

#datdemo_final0<- datdemo_final[,c(2,1,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25)]
#
#
#l.sort <- datdemo_final[order(datdemo_final$vista_lastname),]
#
# demo_wide <- reshape(l.sort, 
#              timevar = "visit_number",
#              idvar = c( "vista_lastname"),
#              direction = "wide")
#
# #to see if responses change between visits 1 and 2
# demo_wide$gen_F_TO_F<- ifelse(demo_wide$demo_gender_r.1 == 2 & demo_wide$demo_gender_r.2 == 2 , "1", "0")
# demo_wide$gen_M_TO_M<- ifelse(demo_wide$demo_gender_r.1 == 1 & demo_wide$demo_gender_r.2 == 1 , "1", "0")
# demo_wide$gen_match<- ifelse(demo_wide$demo_gender_r.1 ==  demo_wide$demo_gender_r.2, "1", "0")
# 
# demo_wide$eth_yes_to_yes<- ifelse(demo_wide$demo_ethnic_r.1 == 1 & demo_wide$demo_ethnic_r.2== 1 , "1", "0")
# demo_wide$eth_no_to_no<- ifelse(demo_wide$demo_ethnic_r.1 == 0 & demo_wide$demo_ethnic_r.2== 0 , "1", "0")
# demo_wide$eth_decline_twice<- ifelse(demo_wide$demo_ethnic_r.1 == 2 & demo_wide$demo_ethnic_r.2== 2 , "1", "0")
# demo_wide$eth_match<- ifelse(demo_wide$demo_ethnic_r.1 ==  demo_wide$demo_ethnic_r.2, "1", "0")
# 
# demo_wide$race_match<- ifelse(demo_wide$race.1 == demo_wide$race.2, "1", "0")
# 
# #GENDER
# sum(as.numeric(demo_wide$gen_F_TO_F), na.rm=TRUE)
# sum(as.numeric(demo_wide$gen_M_TO_M), na.rm=TRUE)
# sum(as.numeric(demo_wide$gen_match), na.rm=TRUE)
# 
# #ETHNICITY
# sum(as.numeric(demo_wide$eth_yes_to_yes), na.rm=TRUE)
# sum(as.numeric(demo_wide$eth_no_to_no), na.rm=TRUE)
# sum(as.numeric(demo_wide$eth_decline_twice), na.rm=TRUE)
# sum(as.numeric(demo_wide$eth_match), na.rm=TRUE)
# 
# #RACE
# sum(as.numeric(demo_wide$race_match), na.rm=TRUE)
# 
# #BETWEEN VISITS 2 And 3
# demo_wide$gen_match_23<- ifelse(demo_wide$demo_gender_r.2 ==  demo_wide$demo_gender_r.3, "1", "0")
# demo_wide$eth_match_23<- ifelse(demo_wide$demo_ethnic_r.2 ==  demo_wide$demo_ethnic_r.3, "1", "0")
# demo_wide$race_match_23<- ifelse(demo_wide$race.2 == demo_wide$race.3, "1", "0")
# 
# #CONSISTENCY VALUE
# demo_wide$consistency23<- ifelse(demo_wide$race_match_23 == "1" &  demo_wide$eth_match_23 =="1" & demo_wide$gen_match_23 =="1" , "1", "0")
# demo_wide$consistency12<- ifelse(demo_wide$race_match == "1" &  demo_wide$eth_match =="1" & demo_wide$gen_match=="1", "1", "0")
# 
# #CONSISTENCY VALUE
# demo_wide$inconsistency12<- ifelse(demo_wide$race_match == "0" |  demo_wide$eth_match =="0" | demo_wide$gen_match=="0", "1", "0")
# demo_wide$inconsistency23<- ifelse(demo_wide$race_match_23 == "0" |  demo_wide$eth_match_23 =="0" | demo_wide$gen_match_23 =="0" , "1", "0")
# 
# 
# #TOTAL CONSISTENCY 
# sum(as.numeric(demo_wide$consistency12), na.rm=TRUE)
# sum(as.numeric(demo_wide$consistency23), na.rm=TRUE)
# 
# 
# #TOTAL INCONSISTENCY 
# demo_wide$inconsistent_score<- sum(as.numeric(demo_wide$inconsistency12), na.rm=TRUE)
# demo_wide$inconsistent_score<- sum(as.numeric(demo_wide$inconsistency23), na.rm=TRUE)
# 


#________________________________________________________________________________________ 
#Export datBTBISa
#----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/5_Basic_Demographic/Basic_Demographic_scored_data_export.csv", sep="")
write.csv(datdemo_final, filename,quote=T,row.names=F,na="NA")
# 
# filename <- paste("~/Biobank/5_Basic_Demographic/Basic_Demographic_wide.csv", sep="")
# write.csv(demo_wide , filename,quote=T,row.names=F,na="NA")
# 


filename <- paste("~/Biobank/5_Basic_Demographic/Basic_Demographic_scored_data_export_DEIDENTIFIED.csv", sep="")
write.csv(datdemo_final1, filename,quote=T,row.names=F,na="NA")

return(print("5_Basic_Demographic_done"))
}

