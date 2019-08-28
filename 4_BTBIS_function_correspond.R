#########################################################################################
# Last datBTBISe modified: 04/12/2018
# Author: Katy Torres
# Description: Subset of question 4, 4_BTBIS
##########################################################################################
btbis <- function(dat0, exportdate)
{
#Load plyr library
library(plyr)


#Only retain relevant variables
datBTBIS <- subset(dat0, select= c(assessment_id,vista_lastname,visit_number, veteran,
                              tbi_blast_BB,
                              tbi_vehicle_BB,
                              tbi_fragment_BB,
                              tbi_fall_BB,
                              tbi_blow_BB,
                              tbi_otherinj_BB,
                              tbi_none_BB,
                              
                              tbi_immed_loss_BB,
                              tbi_immed_dazed_BB,
                              tbi_immed_memory_BB,
                              tbi_immed_concussion_BB,
                              tbi_immed_headinj_BB,
                              tbi_immed_none_BB,
                              tbi_immed_na_BB,
                              
                              tbi_worse_memory_BB,
                              tbi_worse_balance_BB,
                              tbi_worse_light_BB,
                              tbi_worse_irritable_BB,
                              tbi_worse_headache_BB,
                              tbi_worse_sleep_BB,
                              tbi_worse_none_BB,
                              tbi_worse_na_BB,
                              
                              tbi_week_memory_BB,
                              tbi_week_balance_BB,
                              tbi_week_light_BB,
                              tbi_week_irritable_BB,
                              tbi_week_headache_BB,
                              tbi_week_sleep_BB,
                              tbi_week_none_BB,
                              tbi_week_na_BB
              ))


#________________________________________________________________________________________              
# SCORING Functions Defined
#----------------------------------------------------------------------------------------
score_BTBIS <- function(x)
{
  
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
  
  #if subject is a non-veteran the question should not be answered
  invalid_data<- ifelse(veteran == "Y", 0, 1)
  
   #if any of these are true: then q1_yes <- 1
  if(!((is.na(tbi_blast_BB))|(is.na(tbi_vehicle_BB))| (is.na(tbi_fragment_BB))|(is.na(tbi_fall_BB))|(is.na(tbi_blow_BB))|(is.na(tbi_otherinj_BB)))){
    if(tbi_blast_BB==1 | tbi_vehicle_BB==1 |tbi_fragment_BB==1 | tbi_fall_BB==1 |tbi_blow_BB==1 | tbi_otherinj_BB==1 )
    {q1_yes <- 1}
    else{q1_yes <- 0}
  }else{q1_yes <-NA}
    
  #if any of these are true: then q2_yes_ 1
  if(!((is.na(tbi_immed_loss_BB))|(is.na(tbi_immed_dazed_BB))| (is.na(tbi_immed_memory_BB))|(is.na(tbi_immed_concussion_BB))|(is.na(tbi_immed_headinj_BB)))){
    if(tbi_immed_loss_BB==1 | tbi_immed_dazed_BB==1 |tbi_immed_memory_BB==1 | tbi_immed_concussion_BB==1 |tbi_immed_headinj_BB==1 )
    {q2_yes <- 1}
    else{q2_yes <- 0}
  }else{q2_yes <-NA}
  
  #if any of these are true: then q3_yes
  if(!((is.na(tbi_worse_memory_BB))|(is.na(tbi_worse_balance_BB))|(is.na(tbi_worse_light_BB))|(is.na(tbi_worse_irritable_BB))|(is.na(tbi_worse_headache_BB))|(is.na(tbi_worse_sleep_BB)))){
    if(tbi_worse_memory_BB==1 | tbi_worse_balance_BB==1 |tbi_worse_light_BB==1 | tbi_worse_irritable_BB==1 |tbi_worse_headache_BB==1 |tbi_worse_sleep_BB==1)
    {q3_yes <- 1}
    else{q3_yes <- 0}
  }else{q3_yes <-NA}
  
  #if any of these are true: then q2_yes_ 1
  if(!((is.na(tbi_week_memory_BB))|(is.na(tbi_week_balance_BB))| (is.na(tbi_week_light_BB))|(is.na(tbi_week_irritable_BB))|(is.na(tbi_week_headache_BB))|(is.na(tbi_week_sleep_BB)))){
    if(tbi_week_memory_BB==1 | tbi_week_balance_BB==1 |tbi_week_light_BB==1 | tbi_week_irritable_BB==1 |tbi_week_headache_BB==1 |tbi_week_sleep_BB==1)
    {q4_yes <- 1}
    else{q4_yes <- 0}
  }else{q4_yes <-NA}
  
  
  #if both questions 1 and 2 are yes

  if (is.na(q1_yes) & is.na(q2_yes) )
  {
    q1andq2_yes <- NA
  } else if (q1_yes == 1 & is.na(q2_yes)  )
  { 
    q1andq2_yes <- NA
  } else if (is.na(q1_yes) & q2_yes == 1  )
  {
    q1andq2_yes <- NA
  } else if (q1_yes == 1 & q2_yes == 1  )
  {
    q1andq2_yes <- 1
  } else if (q1_yes == 0 & is.na(q2_yes)  )
  {
    q1andq2_yes <- NA
  } else if (is.na(q1_yes) & q2_yes == 0  )
  {
    q1andq2_yes <- NA
  } else if (q1_yes == 0 & q2_yes == 0  )
  {
    q1andq2_yes <- 0
  } else if (q1_yes == 0 & q2_yes == 1  )
  {
    q1andq2_yes <- 0
  } else if (q1_yes == 1 & q2_yes == 0 )
  {
    q1andq2_yes <- 0
  } else {q1andq2_yes<- NA}
  
  
  
  #if both questions 1 and 3 are yes
  
  if (is.na(q1_yes) & is.na(q3_yes) )
  {
    q1andq3_yes <- NA
  } else if (q1_yes == 1 & is.na(q3_yes)  )
  { 
    q1andq3_yes <- NA
  } else if (is.na(q1_yes) & q3_yes == 1  )
  {
    q1andq3_yes <- NA
  } else if (q1_yes == 1 & q3_yes == 1  )
  {
    q1andq3_yes <- 1
  } else if (q1_yes == 0 & is.na(q3_yes)  )
  {
    q1andq3_yes <- NA
  } else if (is.na(q1_yes) & q3_yes == 0  )
  {
    q1andq3_yes <- NA
  } else if (q1_yes == 0 & q3_yes == 0  )
  {
    q1andq3_yes <- 0
  } else if (q1_yes == 0 & q3_yes == 1  )
  {
    q1andq3_yes <- 0
  } else if (q1_yes == 1 & q3_yes == 0 )
  {
    q1andq3_yes <- 0
  } else {q1andq3_yes<- NA}
  
  
  #if both questions 1 and 4 are yes
  
  if (is.na(q1_yes) & is.na(q4_yes) )
  {
    q1andq4_yes <- NA
  } else if (q1_yes == 1 & is.na(q4_yes)  )
  { 
    q1andq4_yes <- NA
  } else if (is.na(q1_yes) & q4_yes == 1  )
  {
    q1andq4_yes <- NA
  } else if (q1_yes == 1 & q4_yes == 1  )
  {
    q1andq4_yes <- 1
  } else if (q1_yes == 0 & is.na(q4_yes)  )
  {
    q1andq4_yes <- NA
  } else if (is.na(q1_yes) & q4_yes == 0  )
  {
    q1andq4_yes <- NA
  } else if (q1_yes == 0 & q4_yes == 0  )
  {
    q1andq4_yes <- 0
  } else if (q1_yes == 0 & q4_yes == 1  )
  {
    q1andq4_yes <- 0
  } else if (q1_yes == 1 & q4_yes == 0 )
  {
    q1andq4_yes <- 0
  } else {q1andq4_yes<- NA}
  
  
tbi_tot_week_symotoms <- tbi_blast_BB+
                               tbi_vehicle_BB+
                               tbi_fragment_BB+
                               tbi_fall_BB+
                               tbi_blow_BB+
                               tbi_otherinj_BB+
                               tbi_none_BB+
                               
                               tbi_immed_loss_BB+
                               tbi_immed_dazed_BB+
                               tbi_immed_memory_BB+
                               tbi_immed_concussion_BB+
                               tbi_immed_headinj_BB+
                               tbi_immed_none_BB+
                               tbi_immed_na_BB+
                               
                               tbi_worse_memory_BB+
                               tbi_worse_balance_BB+
                               tbi_worse_light_BB+
                               tbi_worse_irritable_BB+
                               tbi_worse_headache_BB+
                               tbi_worse_sleep_BB+
                               tbi_worse_none_BB+
                               tbi_worse_na_BB+
                               
                               tbi_week_memory_BB+
                               tbi_week_balance_BB+
                               tbi_week_light_BB+
                               tbi_week_irritable_BB+
                               tbi_week_headache_BB+
                               tbi_week_sleep_BB+
                               tbi_week_none_BB+
                               tbi_week_na_BB

# BTBIS_total_incomplete <- sum(c(tbi_blast_BB,tbi_vehicle_BB,tbi_fragment_BB,tbi_fall_BB,tbi_blow_BB,
#                                 tbi_otherinj_BB,tbi_none_BB, tbi_immed_loss_BB,tbi_immed_dazed_BB,tbi_immed_memory_BB,
#                                 tbi_immed_concussion_BB,tbi_immed_headinj_BB,tbi_immed_none_BB,tbi_immed_na_BB,tbi_worse_memory_BB,
#                                 tbi_worse_balance_BB,tbi_worse_light_BB,tbi_worse_irritable_BB,tbi_worse_headache_BB,tbi_worse_sleep_BB,
#                                 tbi_worse_none_BB,tbi_worse_na_BB,tbi_week_memory_BB,tbi_week_balance_BB,tbi_week_light_BB,
#                                 tbi_week_irritable_BB,tbi_week_headache_BB,tbi_week_sleep_BB, tbi_week_none_BB, tbi_week_na_BB),na.rm=T)

data_complete_btbis<- as.numeric(
  sum(
    is.na(
      c(tbi_blast_BB,tbi_vehicle_BB,tbi_fragment_BB,tbi_fall_BB,tbi_blow_BB,
        tbi_otherinj_BB,tbi_none_BB, tbi_immed_loss_BB,tbi_immed_dazed_BB,tbi_immed_memory_BB,
        tbi_immed_concussion_BB,tbi_immed_headinj_BB,tbi_immed_none_BB,tbi_immed_na_BB,tbi_worse_memory_BB,
        tbi_worse_balance_BB,tbi_worse_light_BB,tbi_worse_irritable_BB,tbi_worse_headache_BB,tbi_worse_sleep_BB,
        tbi_worse_none_BB,tbi_worse_na_BB,tbi_week_memory_BB,tbi_week_balance_BB,tbi_week_light_BB,
        tbi_week_irritable_BB,tbi_week_headache_BB,tbi_week_sleep_BB, tbi_week_none_BB, tbi_week_na_BB
      )
    )
  ) == 0
)

data_not_attempted_btbis<- as.numeric(
  sum(
    is.na(
      c(tbi_blast_BB,tbi_vehicle_BB,tbi_fragment_BB,tbi_fall_BB,tbi_blow_BB,
        tbi_otherinj_BB,tbi_none_BB, tbi_immed_loss_BB,tbi_immed_dazed_BB,tbi_immed_memory_BB,
        tbi_immed_concussion_BB,tbi_immed_headinj_BB,tbi_immed_none_BB,tbi_immed_na_BB,tbi_worse_memory_BB,
        tbi_worse_balance_BB,tbi_worse_light_BB,tbi_worse_irritable_BB,tbi_worse_headache_BB,tbi_worse_sleep_BB,
        tbi_worse_none_BB,tbi_worse_na_BB,tbi_week_memory_BB,tbi_week_balance_BB,tbi_week_light_BB,
        tbi_week_irritable_BB,tbi_week_headache_BB,tbi_week_sleep_BB, tbi_week_none_BB, tbi_week_na_BB
      )
    )
  ) == 30
)

completeness_btbis<- "1"
if(!(is.na(data_not_attempted_btbis))){
  if(data_not_attempted_btbis==1)
  {
    completeness_btbis <- "not attempted"}else{}
}else{completeness_btbis<-NA}

if(!(is.na(data_complete_btbis))){
  if(data_complete_btbis==1){
    completeness_btbis <- "complete"} else{}
}else{completeness_btbis<-NA}

if(data_not_attempted_btbis==0 & data_complete_btbis==0){
  completeness_btbis <- "partially completed"}else{}


scores <- data.frame(q1_yes, q2_yes, q1andq2_yes, q3_yes, q1andq3_yes, q4_yes, q1andq4_yes, tbi_tot_week_symotoms,completeness_btbis, invalid_data )

return(scores)
}

#Calculate summary scores in datBTBISa
score_datBTBIS <- adply(datBTBIS, 1, score_BTBIS)

# based on variable values
newdata <- score_datBTBIS[ which(score_datBTBIS$invalid_data==0), ]

#to anonymize data
newdata1<- within(newdata,
       {
         assessment_id <- NULL
         vista_lastname <- NULL
       })

# based on variable values
newdata <- score_datBTBIS[ which(score_datBTBIS$invalid_data==0), ]

#________________________________________________________________________________________              
# Descriptive Stats and plots
#----------------------------------------------------------------------------------------
#subset by visit to get report information
v1 <- newdata[ which(newdata$visit_number==1), ]
v2 <- newdata[ which(newdata$visit_number==2), ]
v3 <- newdata[ which(newdata$visit_number==3), ]

#completeness table
table(newdata$completeness_btbis, newdata$visit_number)


#------------------------------------------------------------------------
#DATA TRANSFORMATION to see concordance between visits 
#------------------------------------------------------------------------

newdata<- newdata[,c(2,1,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25, 26, 27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43)]


l.sort <- newdata[order(newdata$vista_lastname),]

demo_wide <- reshape(l.sort,
                     timevar = "visit_number",
                     idvar = c( "vista_lastname"),
                     direction = "wide")

#to see if responses change between visits 1 and 2
demo_wide$matchv1v2<- ifelse(demo_wide$q1_yes.1  ==  demo_wide$q1_yes.2, "1", "0")
demo_wide$yes_to_yes<- ifelse(demo_wide$q1_yes.1 == 1 & demo_wide$q1_yes.2== 1 , "1", "0")
demo_wide$no_to_no<- ifelse(demo_wide$q1_yes.1 == 0 & demo_wide$q1_yes.2== 0 , "1", "0")



# 
#   
  #________________________________________________________________________________________ 
  #Export datBTBISa
  #----------------------------------------------------------------------------------------
  filename <- paste("~/Biobank/4_BTBIS/BTBIS_scored_data_export_correspond_per_question.csv", sep="")
  write.csv(mrged, filename,quote=T,row.names=F,na="#N/A")

  
  filename <- paste("~/Biobank/4_BTBIS/BTBIS_scored_data_export.csv", sep="")
  write.csv( newdata, filename,quote=T,row.names=F,na="#N/A")
  
  filename <- paste("~/Biobank/4_BTBIS/BTBIS_scored_data_export_DEIDENTIFIED.csv", sep="")
  write.csv( newdata1, filename,quote=T,row.names=F,na="#N/A")
  
  
  print("4_BTBIS_done")
  
  #return completness column
  myvars <- c("assessment_id", "completeness_btbis")
  newdata <- score_datBTBIS[myvars]
  return(newdata)
}
