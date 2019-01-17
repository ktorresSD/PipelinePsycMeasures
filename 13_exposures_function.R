#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 13, exposures and scoring functions
##########################################################################################

exposures<- function(dat0, exportdate)
{
  dat<-dat0[dat0$visit_number==1,]
#Only retain relevant variables
datexpo <- subset(dat, 
              select= c(assessment_id,vista_lastname, visit_number,
                        serv_exposed,
                        serv_exp_none,
                        serv_exp_chemical,
                        serv_exp_bio,
                        serv_exp_jp8,
                        serv_exp_asbestos,
                        serv_exp_nerve,
                        serv_exp_radio,
                        serv_exp_sand,
                        serv_exp_uranium,
                        serv_exp_industrial,
                        serv_exp_fumes,
                        serv_exp_paint,
                        serv_exp_bite,
                        serv_exp_burn,
                        serv_exp_pest,
                        serv_exp_other,
                        serv_exp_oth1spec,
                        serv_exp_other,
                        serv_exp_oth2spec,
                        
                        serv_animal_bite,
                        serv_animal_blood,
                        serv_animal_bat,
                        
                        serv_combat,
                        serv_comb_none,
                        serv_comb_attack,
                        serv_comb_fire,
                        serv_comb_hand,
                        serv_comb_wounded,
                        serv_comb_interro,
                        serv_comb_rocket,
                        serv_comb_seebody,
                        serv_comb_clear,
                        serv_comb_ship,
                        serv_comb_detain,
                        serv_comb_recdfire,
                        serv_comb_handbody,
                        serv_comb_killed,
                        serv_comb_enemy,
                        Exposures.formula
              ))
#________________________________________________________________________________________ 
# Completeness Check
#----------------------------------------------------------------------------------------

score_expo <- function(x)
{
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])

  
  if(!(is.na(serv_exposed))){
    if(serv_exposed==0)
    {
      exposed <- "no"}else{exposed <- "yes"}
  }else{exposed<-NA}
  
  
  
  if(!(is.na(serv_exp_none))){  
    if(serv_exp_none == 1 | serv_exp_chemical  == 1 | serv_exp_bio == 1 |
       serv_exp_jp8 == 1 | serv_exp_asbestos == 1 | serv_exp_nerve == 1 |
       serv_exp_radio == 1 | serv_exp_sand ==1 | serv_exp_uranium == 1 | 
       serv_exp_fumes == 1 | serv_exp_paint == 1 | serv_exp_bite == 1 |
       serv_exp_industrial == 1 | serv_exp_burn == 1 | serv_exp_burn == 1 |  
       serv_exp_pest == 1 | serv_exp_other == 1){yesexposure <- "yes"
    }else{yesexposure <- "no"}
  }else{yesexposure<-"no"}
  
  
  data_complete_expo<- as.numeric(
    sum(
      is.na(
        c(serv_exposed,
          serv_animal_bite,
          serv_animal_blood,
          serv_animal_bat,
          
          serv_combat,
          serv_comb_none,
          serv_comb_attack,
          serv_comb_fire,
          serv_comb_hand,
          serv_comb_wounded,
          serv_comb_interro,
          serv_comb_rocket,
          serv_comb_seebody,
          serv_comb_clear,
          serv_comb_ship,
          serv_comb_detain,
          serv_comb_recdfire,
          serv_comb_handbody,
          serv_comb_killed,
          serv_comb_enemy
        )
      )
    ) == 0
  )
  
  data_not_attempted_expo<- as.numeric(
    sum(
      is.na(
        c(serv_exposed,
          serv_exp_none,
          serv_exp_chemical,
          serv_exp_bio,
          serv_exp_jp8,
          serv_exp_asbestos,
          serv_exp_nerve,
          serv_exp_radio,
          serv_exp_sand,
          serv_exp_uranium,
          serv_exp_industrial,
          serv_exp_fumes,
          serv_exp_paint,
          serv_exp_bite,
          serv_exp_burn,
          serv_exp_pest,
          serv_exp_other,

          serv_animal_bite,
          serv_animal_blood,
          serv_animal_bat,
          
          serv_combat,
          serv_comb_none,
          serv_comb_attack,
          serv_comb_fire,
          serv_comb_hand,
          serv_comb_wounded,
          serv_comb_interro,
          serv_comb_rocket,
          serv_comb_seebody,
          serv_comb_clear,
          serv_comb_ship,
          serv_comb_detain,
          serv_comb_recdfire,
          serv_comb_handbody,
          serv_comb_killed,
          serv_comb_enemy
        )
      )
    ) == 36
  )
  
  completeness_expo<- "1"
  if(!(is.na(data_not_attempted_expo))){
    if(data_not_attempted_expo==1)
    {
      completeness_expo <- "not attempted"}else{}
  }else{completeness_expo<-NA}
  
  if(!(is.na(data_complete_expo))){
    if(data_complete_expo==1 & yesexposure=="yes"){
      completeness_expo <- "complete"} 
    
    else if(data_complete_expo==1 & exposed=="no"){
      completeness_expo <- "complete"} 
    
    else if(data_complete_expo==1 & yesexposure=="no"){
      completeness_expo <- "partially complete"} 
    else if(data_complete_expo==0 & yesexposure=="yes"){
      completeness_expo <- "partially complete"}
    else{}
  }else{completeness_expo<-NA}
  
  if(data_not_attempted_expo==0 & data_complete_expo==0 & yesexposure=="no"){
    completeness_expo <- "partially complete"}else{}
  
  
  scoresces <- data.frame(exposed, data_not_attempted_expo, data_complete_expo, yesexposure, completeness_expo)
  
  return(scoresces)
  
}

#Calculate summary scores in datBTBISa
score_datexpo <- adply(datexpo, 1, score_expo)

#to anonymize data
score_datexpo1<- within(score_datexpo,
                       {
                         assessment_id <- NULL
                         vista_lastname <- NULL
                       })

#________________________________________________________________________________________ 
#Export
#----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/13_exposures/exposures_scored_data_export.csv", sep="")
write.csv(score_datexpo , filename,quote=T,row.names=F,na="NA")

filename <- paste("~/Biobank/13_exposures/exposures_scored_data_export_DEIDENTIFIED.csv", sep="")
write.csv(score_datexpo1 , filename,quote=T,row.names=F,na="NA")

print("13_Exposures_done")

#return completness column
myvars <- c("assessment_id", "completeness_expo")
newdata <- score_datexpo[myvars]
return(newdata)
}

