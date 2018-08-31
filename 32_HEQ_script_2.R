#########################################################################################
# Last Date modified: 08/31/2018
# Author: Katy Torres
# Description: HEQQ scoring function - paper form questionnaire
##########################################################################################
HEQfunc<- function(dat0, exportdate)
{

  #dat0<- read.csv('C:/Users/Nievergelt Lab/Documents/Biobank/32_HEQ/HEQ_August_test.csv',header=T, na.strings=c("",NA))
  
  #Load libraries
  library(plyr)
  library(ggplot2)
  
  #Only retain relevant variables
  datheq<- subset(dat0, 
                     select= c(assessment_id,vista_lastname,
                               HEQa1_routine,
                               HEQa2_late,
                               HEQa3_after,
                               HEQa4_pickup,
                               HEQa5_track,
                               HEQa6_meal,
                               HEQa7_sleep,
                               HEQa8_bedtime,
                               HEQa9_afterschool,
                               HEQa10_home,
                               HEQa11_event,
                               HEQa12_homework,
                               HEQb1_progress,
                               HEQb2_punish,
                               HEQb3_household,
                               HEQb4_wondered,
                               HEQb5_people,
                               HEQb6_doing,
                               HEQb7_activities,
                               HEQb8_plan,
                               HEQb9_tradition,
                               HEQb10_long,
                               HEQb11_custody,
                               HEQb12_moved,
                               HEQb13_changejob,
                               HEQb14_unemployed,
                               HEQb15_eat,
                               HEQb16_necessities,
                               HEQb17_safe,
                               HEQb18_frequent,
                               HEQb19_midyear,
                               HEQb20_stable,
                               HEQb21_divorced,
                               HEQb22_partner,
                               HEQb23_disorganized,
                               HEQb24_unpredictable,
                               HEQb25_act,
                               HEQb26_furious,
                               HEQb27_stressed,
                               HEQb28_clean,
                               HEQb29_cluttered,
                               HEQb30_noisy,
                               HEQb31_misplaced))

# Data Manipulation and cleaning
#----------------------------------------------------------------------------------------
#Reverse_code the following variables
datheq$q1  <- as.numeric(!datheq$HEQa1_routine)
datheq$q3  <- as.numeric(!datheq$HEQa3_after)
datheq$q5  <- as.numeric(!datheq$HEQa5_track)
datheq$q6  <- as.numeric(!datheq$HEQa6_meal)
datheq$q7  <- as.numeric(!datheq$HEQa7_sleep)
datheq$q8  <- as.numeric(!datheq$HEQa8_bedtime)
datheq$q9  <- as.numeric(!datheq$HEQa9_afterschool)
datheq$q10 <- as.numeric(!datheq$HEQa10_home)
datheq$q11 <- as.numeric(!datheq$HEQa11_event)
datheq$q12 <- as.numeric(!datheq$HEQa12_homework)
datheq$q1b <- as.numeric(!datheq$HEQb1_progress)
datheq$q6b <- as.numeric(!datheq$HEQb6_doing)
datheq$q7b <- as.numeric(!datheq$HEQb7_activities)
datheq$q9b <- as.numeric(!datheq$HEQb9_tradition)
datheq$q20b<- as.numeric(!datheq$HEQb20_stable)
datheq$q28b<- as.numeric(!datheq$HEQb28_clean)

#check reversing ocde worked 
cbind(datheq$HEQa5_track, datheq$q5)

#________________________________________________________________________________________
# Missing Data Imputation functions defined
#----------------------------------------------------------------------------------------
#12 AND UNDER IMPUTATION
age012_score <- function(y)
  {
    W<-y[,c("q1","q3","q5","q6","q7","q8","q9","q10","q11","q12","HEQa2_late", "HEQa4_pickup")]
    for (v in 1:length(W)) assign(names(W)[v], W[[v]])
    
    na_count <- apply(W, 1, function(i) sum(is.na(i)))
    #print(na_count)
    if(na_count<= 2 && na_count>0 )
    {
      mn<- rowMeans(W, na.rm=TRUE) 
      #print(mn)
      W[is.na(W)] <- mn
      W$imputed <- 1
      #print(W)
      #print("imputed row")
    }else{ W$imputed <- 0 }
    
    age12_total_score <- W$q1+ W$q3+ W$q5+ W$q6+ W$q7+ W$q8+ W$q9+ W$q10+ W$q11+ W$q12+ W$HEQa2_late+ W$HEQa4_pickup
    #print(age12_total_score)
    
    data_complete_age12<- as.numeric(
      sum(
        is.na(
          c(q1,q3,q5,q6,q7,q8,q9,q10,q11,q12,HEQa2_late, HEQa4_pickup
          )
        )
      ) == 0
    )
    scores <- data.frame(age12_total_score, data_complete_age12 )
    return(scores)
  
  }
  
  #Calculate summary scores in data 
Age012_scored <- adply(datheq, 1, age012_score)
  



#PARENTAL IMPUTATION
paren_score <- function(y)
  {
    
    py<-y[,c("HEQb23_disorganized",	"HEQb24_unpredictable",	"HEQb25_act",	"HEQb26_furious",	"HEQb27_stressed")]
    for (v in 1:length(py)) assign(names(py)[v], py[[v]])
    
    na_count <- apply(py, 1, function(i) sum(is.na(i)))
    if(na_count<= 1 && na_count>0 )
    {
      mn<- rowMeans(py, na.rm=TRUE) 
      py[is.na(py)] <- mn
      py$imputed <- 1
      print("missing data in this row")
    }else{ py$imputed <- 0}
    
    paren_total_score <- py$HEQb23_disorganized+	py$HEQb24_unpredictable+	py$HEQb25_act+	py$HEQb26_furious+	py$HEQb27_stressed
    
    data_complete_paren<- as.numeric(
      sum(is.na(c(HEQb23_disorganized,	HEQb24_unpredictable,	HEQb25_act,	HEQb26_furious,	HEQb27_stressed))) == 0)
    scores <- data.frame(paren_total_score, data_complete_paren)
    return(scores)
  }
  
  #Calculate summary scores in data 
  paren_scored <- adply(Age012_scored, 1, paren_score)
  
  
  
#HOUSEHOLD IMPUTATION
  househ_score <- function(y)
  {
    hy<-y[,c("q28b","HEQb29_cluttered","HEQb30_noisy","HEQb31_misplaced")]
    for (v in 1:length(hy)) assign(names(hy)[v], hy[[v]])
    
    na_count <- apply(hy, 1, function(i) sum(is.na(i)))
    if(na_count<= 1 && na_count>0 )
    {
      mn<- rowMeans(hy, na.rm=TRUE) 
      hy[is.na(hy)] <- mn
      hy$imputed <- 1
      #print("missing data in this row")
    }else{ hy$imputed <- 0}
    
    househ_total_score <- hy$q28b + hy$HEQb29_cluttered + hy$HEQb30_noisy + hy$HEQb31_misplaced
    
    data_complete_househ<- as.numeric(
      sum(is.na(c(q28b,HEQb29_cluttered,HEQb30_noisy,HEQb31_misplaced))) == 0)
    scores <- data.frame(househ_total_score, data_complete_househ)
    return(scores)
  }
  
  #Calculate summary scores in data 
  househ_scored <- adply(paren_scored, 1, househ_score)

  
#overall score
  #should ONLY BE IMPUTED IF NO MORE THAN 6 ARE MISSING
  
  
  overall_score <- function(y)
  {
    for (v in 1:length(y)) assign(names(y)[v], y[[v]])
    
    
    HEQ_overall_score <- q1 + HEQa2_late + q3 + HEQa4_pickup + q5 + q6 + q7 + q8 + q9 + q10 +
      q11 + q12 + q1b + HEQb2_punish + HEQb3_household + HEQb4_wondered + HEQb5_people +
      q6b + q7b + HEQb8_plan + q9b + HEQb10_long + HEQb11_custody + HEQb12_moved + 
      HEQb13_changejob + HEQb14_unemployed + HEQb15_eat + HEQb16_necessities + HEQb17_safe + 
      HEQb18_frequent + HEQb19_midyear + q20b + HEQb21_divorced + HEQb22_partner + 
      HEQb23_disorganized + HEQb24_unpredictable + HEQb25_act +  HEQb26_furious + HEQb27_stressed + 
      q28b + HEQb29_cluttered + HEQb30_noisy + HEQb31_misplaced
    
    HEQ_overall_score_incomp <- sum(c(q1 , HEQa2_late , q3 , HEQa4_pickup , q5 , q6 , q7 , q8 , q9 , q10 ,
                                      q11 , q12 , q1b , HEQb2_punish , HEQb3_household , HEQb4_wondered , HEQb5_people ,
                                      q6b , q7b , HEQb8_plan , q9b , HEQb10_long , HEQb11_custody, HEQb12_moved , 
                                        HEQb13_changejob , HEQb14_unemployed , HEQb15_eat , HEQb16_necessities , HEQb17_safe , 
                                        HEQb18_frequent , HEQb19_midyear, q20b , HEQb21_divorced , HEQb22_partner , 
                                        HEQb23_disorganized , HEQb24_unpredictable , HEQb25_act ,  HEQb26_furious , HEQb27_stressed, 
                                      q28b , HEQb29_cluttered , HEQb30_noisy , HEQb31_misplaced), na.rm = TRUE)
    
    #COMPLETENESS CHECK
    data_complete_heq<- as.numeric(
      sum(is.na(c(q1 , HEQa2_late , q3 , HEQa4_pickup , q5 , q6 , q7 , q8 , q9 , q10 ,
                    q11 , q12 , q1b , HEQb2_punish , HEQb3_household , HEQb4_wondered , HEQb5_people ,
                    q6b , q7b , HEQb8_plan , q9b , HEQb10_long , HEQb11_custody ,  HEQb12_moved , 
                  HEQb13_changejob , HEQb14_unemployed , HEQb15_eat , HEQb16_necessities , HEQb17_safe , 
                  HEQb18_frequent , HEQb19_midyear, q20b , HEQb21_divorced , HEQb22_partner , 
                  HEQb23_disorganized , HEQb24_unpredictable , HEQb25_act ,  HEQb26_furious , HEQb27_stressed, 
                  q28b , HEQb29_cluttered , HEQb30_noisy , HEQb31_misplaced))) == 0)
    
    data_not_attempted_heq<- as.numeric(
      sum(is.na(c(q1 , HEQa2_late , q3 , HEQa4_pickup , q5 , q6 , q7 , q8 , q9 , q10 ,
                  q11 , q12 , q1b , HEQb2_punish , HEQb3_household , HEQb4_wondered , HEQb5_people ,
                  q6b , q7b , HEQb8_plan , q9b , HEQb10_long , HEQb11_custody ,  HEQb12_moved , 
                  HEQb13_changejob , HEQb14_unemployed , HEQb15_eat , HEQb16_necessities , HEQb17_safe , 
                  HEQb18_frequent , HEQb19_midyear, q20b , HEQb21_divorced , HEQb22_partner , 
                  HEQb23_disorganized , HEQb24_unpredictable , HEQb25_act ,  HEQb26_furious , HEQb27_stressed, 
                  q28b , HEQb29_cluttered , HEQb30_noisy , HEQb31_misplaced))) == 43)
    
    
    completeness_heq<- "1"
    if(!(is.na(data_not_attempted_heq))){
      if(data_not_attempted_heq==1)
      {
        completeness_heq <- "not attempted"}else{}
    }else{completeness_heq<-NA}
    
    if(!(is.na(data_complete_heq))){
      if(data_complete_heq==1){
        completeness_heq <- "complete"} else{}
    }else{completeness_heq<-NA}
    
    if(data_not_attempted_heq==0 & data_complete_heq==0 ){
      completeness_heq <- "partially completed"}else{}
    
    
    scores <- data.frame(HEQ_overall_score, HEQ_overall_score_incomp, data_complete_heq, completeness_heq)
    return(scores)
  }
  
  #Calculate summary scores in data 
  overall_scored <- adply(househ_scored, 1, overall_score)

  #to anonymize data
  overall_scored1<- within( overall_scored,
                           {
                             assessment_id <- NULL
                             vista_lastname <- NULL
                           })
  
  # #________________________________________________________________________________________ 
  # #Export data
  # #----------------------------------------------------------------------------------------
  filename <- paste("~/Biobank/32_HEQ/HEQ_reduced_data_export.csv", sep="")
  write.csv(overall_scored, filename,quote=T,row.names=F,na="#N/A")
  
  filename <- paste("~/Biobank/32_HEQ/HEQ_reduced_data_export_DEIDENTIFIED.csv", sep="")
  write.csv(overall_scored1, filename,quote=T,row.names=F,na="#N/A")
  
  print("HEQ_done")
  
  #return completness column
  myvars <- c("assessment_id", "completeness_heq")
  newdata <- overall_scored[myvars]
  return(newdata)
}



