#########################################################################################
# Last Date modified: 10/22/2018
# Author: Katy Torres
# Description: HEQQ scoring function - paper form questionnaire
##########################################################################################
HEQfunc<- function(dat0, exportdate)
{
 #dat <- read.csv("C:/Users/Nievergelt Lab/Documents/Biobank/32_HEQ/HEQ_reduced_data_export.CSV")
 dat<-dat0[dat0$visit_number==1,]
  
  #Load libraries
  library(plyr)
  library(ggplot2)
  library(matrixStats)
  
  #Only retain relevant variables
  datheq<- subset(dat, 
                     select= c(assessment_id,vista_lastname, visit_number,
                               HEQ1_rel,
                               HEQ0_rel,
                               HEQ1_rel_2,
                               HEQ1_rel_3,
                               HEQ1_rel_4,
                               HEQ1_rel,
                               
                               HEQa1_routine,
                               HEQa4_pickup,
                               HEQa5_track,
                               HEQa6_meal,
                               HEQa7_sleep,
                               HEQa8_bedtime,
                               HEQa9_afterschool,
                               HEQa10_home,
                               HEQa12_homework,
                               
                               HEQb1_progress,
                               HEQb2_punish,
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
                               HEQb31_misplaced,
                               HEQ1_count, HEQ_version

                     ))

# Data Manipulation and cleaning
#----------------------------------------------------------------------------------------
#Reverse_code the following variables
datheq$q1  <- as.numeric(!datheq$HEQa1_routine)
datheq$q3  <- as.numeric(!datheq$HEQa5_track)
datheq$q4  <- as.numeric(!datheq$HEQa6_meal)
datheq$q5  <- as.numeric(!datheq$HEQa7_sleep)
datheq$q6  <- as.numeric(!datheq$HEQa8_bedtime)
datheq$q7  <- as.numeric(!datheq$HEQa9_afterschool)
datheq$q8  <- as.numeric(!datheq$HEQa10_home)
datheq$q9  <- as.numeric(!datheq$HEQa12_homework)

datheq$q10 <- as.numeric(!datheq$HEQb1_progress)

datheq$q14 <- as.numeric(!datheq$HEQb6_doing)
datheq$q15 <- as.numeric(!datheq$HEQb7_activities)
datheq$q17 <- as.numeric(!datheq$HEQb9_tradition)
datheq$q28 <- as.numeric(!datheq$HEQb20_stable)
datheq$q36 <- as.numeric(!datheq$HEQb28_clean)

#Rename non-reverse-coded variables
datheq$q2   <- datheq$HEQa4_pickup
datheq$q11  <- datheq$HEQb2_punish
datheq$q12  <- datheq$HEQb4_wondered
datheq$q13  <- datheq$HEQb5_people

datheq$q16  <- datheq$HEQb8_plan
datheq$q18  <- datheq$HEQb10_long
datheq$q19  <- datheq$HEQb11_custody
datheq$q20  <- datheq$HEQb12_moved
datheq$q21  <- datheq$HEQb13_changejob
datheq$q22  <- datheq$HEQb14_unemployed
datheq$q23  <- datheq$HEQb15_eat
datheq$q24  <- datheq$HEQb16_necessities
datheq$q25  <- datheq$HEQb17_safe
datheq$q26  <- datheq$HEQb18_frequent
datheq$q27  <- datheq$HEQb19_midyear
datheq$q29  <- datheq$HEQb21_divorced
datheq$q30  <- datheq$HEQb22_partner

datheq$q31  <- datheq$HEQb23_disorganized
datheq$q32  <- datheq$HEQb24_unpredictable
datheq$q33  <- datheq$HEQb25_act
datheq$q34  <- datheq$HEQb26_furious
datheq$q35  <- datheq$HEQb27_stressed

datheq$q37  <- datheq$HEQb29_cluttered
datheq$q38  <- datheq$HEQb31_misplaced


#check that it worked
#cbind(datheq$HEQa1_routine,datheq$q1 )

# ________________________________________________________________________________________
# Missing Data Imputation functions defined
# ----------------------------------------------------------------------------------------
#12 AND UNDER IMPUTATION
#Imputes the median for missing items within each scale (up to 2 missing items allowed for Age 0-12)

age012_IMPUTE<- function(y)
  {
    W<-y[,c("q1","q2", "q3", "q4","q5","q6","q7","q8","q9")]
    for (v in 1:length(W)) assign(names(W)[v], W[[v]])

    na_count <- apply(W, 1, function(i) sum(is.na(i)))
    #print(na_count)
    if(na_count<= 2 && na_count>0 )
    {
      mn<- rowMedians(W, na.rm=TRUE) #CHANGE TO THE MEDIAN FOR THAT COLUMN

      W[is.na(W)] <- mn
      W$imputed <- 1
    }else{ W$imputed <- 0 }

    return(W$imputed)
}

#Calculate summary scores in data
Age012_IMPUTED <- adply(datheq, 1, age012_IMPUTE)

# #PARENTAL IMPUTATION
paren_IMPUTE <- function(y)
  {

    py<-y[,c("q31", "q32", "q33", "q34", "q35" )]
    for (v in 1:length(py)) assign(names(py)[v], py[[v]])

    na_count <- apply(py, 1, function(i) sum(is.na(i)))
    if(na_count<= 1 && na_count>0 )
    {
      mn<- rowMedians(py, na.rm=TRUE)
      py[is.na(py)] <- mn
      py$imputed <- 1
      print("missing data in this row")
    }else{ py$imputed <- 0}

    return(py$imputed)
  }

  #Calculate summary scores in data
  paren_IMPUTED <- adply(Age012_IMPUTED, 1, paren_IMPUTE)



#HOUSEHOLD IMPUTATION
  househ_IMPUTE <- function(y)
  {
    hy<-y[,c("q36","q37","q38")]
    for (v in 1:length(hy)) assign(names(hy)[v], hy[[v]])

    na_count <- apply(hy, 1, function(i) sum(is.na(i)))
    if(na_count<= 1 && na_count>0 )
    {
      mn<- rowMedians(hy, na.rm=TRUE)
      hy[is.na(hy)] <- mn
      hy$imputed <- 1
      #print("missing data in this row")
    }else{ hy$imputed <- 0}

    return(hy$imputed)
  }

  #Calculate summary scores in data
  househ_IMPUTED <- adply(paren_IMPUTED, 1,   househ_IMPUTE )

  attach(datheq)

#SUBSCORES
#--------------------------------------------

datheq$Parental_monitoring <- q1 + q3 + q4 + q5 + q6 + q7 + q9 + q10 + q14
datheq$Parental_predictability <- q2 + q8 + q11 + q12 + q15 + q16 + q17 + q31 + q32 + q33 + q34 + q35
datheq$Parental_environment <-  q18 + q19 + q21 + q22 + q28 + q29 + q30
datheq$Physical_environment <- q13 + q20 + q26 + q27 + q36 + q37 + q38
datheq$Safety_security <- q23 + q24 + q25
  

#overall score
#---------------------------------------------
  overall_score <- function(y)
  {
    for (v in 1:length(y)) assign(names(y)[v], y[[v]])
    
    HEQ_overall_score <- Parental_monitoring  +  Parental_predictability +  Parental_environment +  Physical_environment +  Safety_security
    
    HEQ_overall_score_incomp <- sum(c(q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12, q13, q14, q15, q16, q17, 
                                      q18, q19, q20, q21, q22, q23, q24, q25, q26, q27, q28, q29, q30, 
                                      q31, q32, q33, q34, q35, q36, q37, q38), na.rm = TRUE)
    
    #COMPLETENESS CHECK
    data_complete_heq<- as.numeric(
      sum(is.na(c(q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12, q13, q14, q15, q16, q17, 
                  q18, q19, q20, q21, q22, q23, q24, q25, q26, q27, q28, q29, q30, 
                  q31, q32, q33, q34, q35,  q36, q37, q38))) == 0)
    
    data_not_attempted_heq<- as.numeric(
      sum(is.na(c(q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12, q13, q14, q15, q16, q17, 
                  q18, q19, q20, q21, q22, q23, q24, q25, q26, q27, q28, q29, q30, 
                  q31, q32, q33, q34, q35,  q36, q37, q38))) == 38)
    
    
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
  overall_scored <- adply(datheq, 1, overall_score)
  
  names(overall_scored)
  
  # # exclude intermerdiate variables
  # myvars <- names(mydata) %in% c(
  #   "q1"," q3", "q4", "q5", "q6", "q7", "q8", "q9", 
  #  "q10"," q14", "q15", "q17", "q28", "q36", "q2", "q11", 
  #  "q12", "q13", "q16", "q18", "q19", "q20", "q21", "q22", 
  #  "q23", "q24", "q25", "q26", "q27", "q29", "q30", "q31", 
  #  "q32", "q33", "q34", "q35", "q37", "q38")
  #   
  # newdata <- overall_scored[!overall_scored]

  

  #to anonymize data
  overall_scored1<- within( overall_scored,
                           {
                             assessment_id <- NULL
                             vista_lastname <- NULL
                           })
  
  
  #For Report
  #---------------------------------------------------------------------------
  #completeness table
  table(overall_scored$completeness_heq)
  
  library(psych)
  
  #summary statistics
  describe(overall_scored$HEQ_overall_score_incomp)
  describe(overall_scored$Parental_monitoring)
  describe(overall_scored$Parental_predictability)
  describe(overall_scored$Parental_environment)  
  describe(overall_scored$Physical_environment)  
  describe(overall_scored$Safety_security)  

  
  
  hist(overall_scored$HEQ_overall_score_incomp, ylim=c(0,70), xlim = c(0,40),
       xlab = "Overall HEQ score", col = c("steelblue3"), main = "Assessment Count of Total Overall HEQ Score")
  
  hist(overall_scored$Parental_monitoring, ylim=c(0,70), xlim = c(0,9),
       xlab = "Sum Scores of Parental monitoring", col = c("steelblue3"), main = "Assessment Count of Parental monitoring subscale")
  
  hist(overall_scored$Parental_predictability, ylim=c(0,70), xlim = c(0,12),
       xlab = "Sum Scores of Parental_predictability", col = c("steelblue3"), main = "Assessment Count of Parental predictability subscale")
  
  hist(overall_scored$Parental_environment, ylim=c(0,70), xlim = c(0,7),
       xlab = "Sum Scores of Parental_environment", col = c("steelblue3"), main = "Assessment Count of Parental environment subscale")
  
  hist(overall_scored$Physical_environment, ylim=c(0,70), xlim = c(0,7),
       xlab = "Sum Scores of Physical_environment", col = c("steelblue3"), main = "Assessment Count of Physical environment subscale")
  
  hist(overall_scored$Safety_security, ylim=c(0,70),
       xlab = "Sum Scores of Safety and security", col = c("steelblue3"), main = "Assessment Count of Safety and security subscale")
  
  
  #________________________________________________________________________________________
  #Export data
  #----------------------------------------------------------------------------------------
  filename <- paste("~/Biobank/32_HEQ/HEQ_reduced_data_export.csv", sep="")
  write.csv(overall_scored, filename,quote=T,row.names=F,na="NA")
  
  filename <- paste("~/Biobank/32_HEQ/HEQ_reduced_data_export_DEIDENTIFIED.csv", sep="")
  write.csv(overall_scored1, filename,quote=T,row.names=F,na="NA")
  
  print("HEQ_done")
  
  #return completness column
  myvars <- c("assessment_id", "completeness_heq")
  newdata <- overall_scored[myvars]
  return(newdata)
}



