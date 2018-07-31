#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 31, WHODAS and scoring functions
##########################################################################################

whodas<- function(dat0, exportdate)
{
  
#Load plyr library
library(plyr)
library(ggplot2)

#Only retain relevant variables
datwhodas <- subset(dat0, 
                    select= c(assessment_id,vista_lastname, visit_number,
                              whodas1_1_concentrate,
                              whodas1_2_remember,
                              whodas1_3_solution,
                              whodas1_4_new,
                              whodas1_5_understand,
                              whodas1_6_conversation,
                              
                              whodas2_1_stand,
                              whodas2_2_standup,
                              whodas2_3_move,
                              whodas2_4_getout,
                              whodas2_5_walk,
                              
                              whodas3_1_wash,
                              whodas3_2_dressed,
                              whodas3_3_eat,
                              whodas3_4_stay,
                              
                              whodas4_1_deal,
                              whodas4_2_friend,
                              whodas4_3_getalong,
                              whodas4_4_newfriend,
                              whodas4_5_sexual,
                              
                              whodas5_1_housecare,
                              whoda5_2_housetask,
                              whodas5_3_housedone,
                              whodas5_4_housequickly,
                              whodas_work,
                              
                              whodas5_5_daily,
                              whodas5_6_workwell,
                              whodas5_7_workdone,
                              whodas5_8_workquickly,
                              
                              whodas6_1_community,
                              whodas6_2_barriers,
                              whodas6_3_dignity,
                              whodas6_4_time,
                              whodas6_5_emotion,
                              whodas6_6_finance,
                              whodas6_7_family,
                              whodas6_8_relax,
                              
                              whodas_understand_mean,
                              whodas_understand_score,
                              
                              whodas_mobility_mean,
                              whodas_mobility_score,
                              
                              whodas_selfcare_mean,
                              whodas_selfcare_score,
                              
                              whodas_people_mean,
                              whodas_people_score,
                              
                              whodas_household_mean,
                              whodas_household_score,
                              
                              whodas_work_mean,
                              whodas_work_score,
                              
                              whodas_society_mean,
                              whodas_society_score
                              
                    ))
#________________________________________________________________________________________
# Missing Data Imputation function defined
#----------------------------------------------------------------------------------------
# In all other situations where one or two items are missing, the mean score across all items within
# the domain should be assigned to the missing items. This method should not be used if more
# than two items are missing. In addition, if domain-wise scores are being computed for domains,
# the two missing items should not come from the same domain.

MissingDat <- function(x)
{
  #Define Domains as subsets of data
  D1<-x[,c("whodas1_1_concentrate" , "whodas1_2_remember" , "whodas1_3_solution" , "whodas1_4_new" , "whodas1_5_understand" , "whodas1_6_conversation")]
  D2<-x[,c("whodas2_1_stand" , "whodas2_2_standup" , "whodas2_3_move" , "whodas2_4_getout" , "whodas2_5_walk")]
  D3<-x[,c("whodas3_1_wash" , "whodas3_2_dressed" , "whodas3_3_eat" , "whodas3_4_stay")]
  D4<-x[,c("whodas4_1_deal" , "whodas4_2_friend" , "whodas4_3_getalong" , "whodas4_4_newfriend" , "whodas4_5_sexual")]
  D51<-x[,c("whodas5_1_housecare" , 
            "whoda5_2_housetask" , "whodas5_3_housedone" , "whodas5_4_housequickly")]
  D52<-x[,c("whodas5_5_daily","whodas5_6_workwell","whodas5_7_workdone","whodas5_8_workquickly")]
  D6<-x[,c("whodas6_1_community" , "whodas6_2_barriers" , "whodas6_3_dignity" , "whodas6_4_time" , "whodas6_5_emotion" , "whodas6_6_finance" , "whodas6_7_family" , "whodas6_8_relax")]
  
  dfList<- list(D1,D2,D3,D4,D51,D52,D6)
  
  #loop each domain and have it count the number of NA's and deal with missing data
  #make a data frame of data frames
  ttt <- lapply(dfList, function(y) {
    #count number of NA's in each domain
    na_count <- apply(y, 1, function(i) sum(is.na(i)))
    
    #if less than 2, make NA's be the mean of other items in domain
    if(na_count <= 2)
    {
      
      mn<- rowMeans(y, na.rm=TRUE) 
      y[is.na(y)] <- mn
      if(na_count >= 1)
      {
        y$imputed <- 1
        print("imputed row")
      } 
      else{
        y$imputed <- 0
      }
      names(y)[which(names(y) == "imputed")] <- paste(names(y)[1],"imputed",sep="_")
    }
    return(y)
  })
  
  #list.cbind(dfList)
  newdat<-do.call(cbind,ttt)
  whodasscores <- data.frame(newdat)
  
  return(whodasscores)
}



# Imputation function called
#test with subject I know needs imputation
#whodas_missing_dealt <- adply(dat[dat$assessment_id==8583,], 1, MissingDat)
whodas_missing_dealt <- adply(datwhodas, 1, MissingDat)

#----------------------------------------------------------------------------------------
#generate new dataset based on original dataset but with columns added for new columns
datawhodas1<-datwhodas
#add imputed column to original data set, stating if the data was imputed or complete
imputed_columns <- whodas_missing_dealt[,grepl( "imputed" , names(whodas_missing_dealt) ) ]
datawhodas1$imputed_data=apply(imputed_columns,1,function(x) ifelse(any(x==1),'1','0'))
datawhodas1$imputed_data[is.na(datawhodas1$imputed_data)] <- 0
#________________________________________________________________________________________              
# SCORING DEFINED
#----------------------------------------------------------------------------------------
#Calculate summary scores in data using the imputed data and add this to original data frame
#NOTE: For summation purposes we will remove all NA's/treat them as zeros



#Deal with non-working individuals
#If the respondent is not working and has given responses to the 32-item WHODAS 2.0, the score
#can be used as it is, and will be comparable to that of the full 36-item version.

#ACTUAL DATA SCORES
#----------------------------------------------------------------------------------------
datawhodas1$whodas_work_score2 <- datawhodas1$whodas_work_score
datawhodas1$whodas_work_score2[is.na(datawhodas1$whodas_work_score2)] <- 0
#add all fields taking into account that some people do not work
datawhodas1$score_sum_actual<- as.numeric(rowSums(datawhodas1[,c("whodas_selfcare_score",
                                                                 "whodas_understand_score",
                                                                 "whodas_society_score",
                                                                 "whodas_household_score",
                                                                 "whodas_work_score2",
                                                                 "whodas_mobility_score",
                                                                 "whodas_people_score")],na.rm=FALSE))


#IMPUTED DATA SCORES
#----------------------------------------------------------------------------------------
whodas_missing_dealt$whodas_work_score2 <- whodas_missing_dealt$whodas_work_score

#SUM THE DOMAIN SCORES WITH IMPUTED DATA
whodas_missing_dealt$D1_IMPUTED_SCORE<-as.numeric(rowSums(whodas_missing_dealt [,c("whodas1_1_concentrate" , "whodas1_2_remember" , "whodas1_3_solution" , "whodas1_4_new" , "whodas1_5_understand" , "whodas1_6_conversation")],na.rm=FALSE))
whodas_missing_dealt$D2_IMPUTED_SCORE<-as.numeric(rowSums(whodas_missing_dealt [,c("whodas2_1_stand" , "whodas2_2_standup" , "whodas2_3_move" , "whodas2_4_getout" , "whodas2_5_walk")],na.rm=FALSE))
whodas_missing_dealt$D3_IMPUTED_SCORE<-as.numeric(rowSums(whodas_missing_dealt [,c("whodas3_1_wash" , "whodas3_2_dressed" , "whodas3_3_eat" , "whodas3_4_stay")],na.rm=FALSE))
whodas_missing_dealt$D4_IMPUTED_SCORE<-as.numeric(rowSums(whodas_missing_dealt [,c("whodas4_1_deal" , "whodas4_2_friend" , "whodas4_3_getalong" , "whodas4_4_newfriend" , "whodas4_5_sexual")],na.rm=FALSE))
whodas_missing_dealt$D51_IMPUTED_SCORE<-as.numeric(rowSums(whodas_missing_dealt [,c("whodas5_1_housecare" , "whoda5_2_housetask" , "whodas5_3_housedone" , "whodas5_4_housequickly")],na.rm=FALSE))
whodas_missing_dealt$D52_IMPUTED_SCORE<-as.numeric(rowSums(whodas_missing_dealt [,c("whodas5_5_daily","whodas5_6_workwell","whodas5_7_workdone","whodas5_8_workquickly")],na.rm=FALSE))
whodas_missing_dealt$D6_IMPUTED_SCORE<-as.numeric(rowSums(whodas_missing_dealt [,c("whodas6_1_community" , "whodas6_2_barriers" , "whodas6_3_dignity" , "whodas6_4_time" , "whodas6_5_emotion" , "whodas6_6_finance" , "whodas6_7_family" , "whodas6_8_relax")],na.rm=FALSE))


#convert NAs due to not working into zeros
whodas_missing_dealt$D52_IMPUTED_SCORE[is.na(whodas_missing_dealt$D52_IMPUTED_SCORE)] <- 0

#add all fields taking into account that some people do not work
datawhodas1$score_sum_imputed <- as.numeric(rowSums(whodas_missing_dealt[,c("D1_IMPUTED_SCORE",
                                                                            "D2_IMPUTED_SCORE",
                                                                            "D3_IMPUTED_SCORE",
                                                                            "D4_IMPUTED_SCORE",
                                                                            "D51_IMPUTED_SCORE",
                                                                            "D52_IMPUTED_SCORE",
                                                                            "D6_IMPUTED_SCORE")],na.rm=FALSE))
#remove temporary variable
whodas_missing_dealt$whodas_work_score2 <- NULL


#Score taking into account people who don't work
#----------------------------------------------------------------------------------------
datawhodas1$work <- "0"

for (i in 1:nrow(datawhodas1)){
  if (datawhodas1$whodas_work_score2[i] == 0){
    datawhodas1$summaryscore_imputed[i]<- datawhodas1$score_sum_imputed[i]*100/166
    datawhodas1$summaryscore_actual[i]<- datawhodas1$score_sum_actual[i]*100/166
    datawhodas1$work[i] <- "non-working"
  } else {
    datawhodas1$summaryscore_imputed[i]<- datawhodas1$score_sum_imputed[i]*100/180
    datawhodas1$summaryscore_actual[i]<- datawhodas1$score_sum_actual[i]*100/180
    datawhodas1$work[i] <- "working"
  }}

#remove temporary variable
datawhodas1$whodas_work_score2 <- NULL

#plot of scores
a <- ggplot(datawhodas1, aes(x =score_sum_imputed))
a + ggtitle("Disability Score by work status") +
  geom_density(aes(fill = work),  alpha = 0.4) 
  

#plot of scores
a <- ggplot(datawhodas1, aes(x =summaryscore_imputed))
a + ggtitle("% Disability by work status") + 
  geom_density(aes(fill = work), alpha = 0.4) 


#plot of scores
a <- ggplot(datawhodas1, aes(x =summaryscore_imputed))
a + ggtitle("Distribuition of % Disability") + 
  geom_density(fill= "lavender") +
  xlab("% Disability")



#________________________________________________________________________________________              
# Completeness check
#----------------------------------------------------------------------------------------
whodas_check <- function(x)
{
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])

  #checking for completeness
data_complete_whodas<- as.numeric(
  sum(
    is.na(
      c(whodas1_1_concentrate,
        whodas1_2_remember,
        whodas1_3_solution,
        whodas1_4_new,
        whodas1_5_understand,
        whodas1_6_conversation,
        
        whodas2_1_stand,
        whodas2_2_standup,
        whodas2_3_move,
        whodas2_4_getout,
        whodas2_5_walk,
        
        whodas3_1_wash,
        whodas3_2_dressed,
        whodas3_3_eat,
        whodas3_4_stay,
        
        whodas4_1_deal,
        whodas4_2_friend,
        whodas4_3_getalong,
        whodas4_4_newfriend,
        whodas4_5_sexual,
        
        whodas5_1_housecare,
        whoda5_2_housetask,
        whodas5_3_housedone,
        whodas5_4_housequickly,
        whodas_work,
        
        whodas6_1_community,
        whodas6_2_barriers,
        whodas6_3_dignity,
        whodas6_4_time,
        whodas6_5_emotion,
        whodas6_6_finance,
        whodas6_7_family,
        whodas6_8_relax
      )
    )
  ) == 0
)

data_not_attempted_whodas<- as.numeric(
  sum(
    is.na(
      c(whodas1_1_concentrate,
        whodas1_2_remember,
        whodas1_3_solution,
        whodas1_4_new,
        whodas1_5_understand,
        whodas1_6_conversation,
        
        whodas2_1_stand,
        whodas2_2_standup,
        whodas2_3_move,
        whodas2_4_getout,
        whodas2_5_walk,
        
        whodas3_1_wash,
        whodas3_2_dressed,
        whodas3_3_eat,
        whodas3_4_stay,
        
        whodas4_1_deal,
        whodas4_2_friend,
        whodas4_3_getalong,
        whodas4_4_newfriend,
        whodas4_5_sexual,
        
        whodas5_1_housecare,
        whoda5_2_housetask,
        whodas5_3_housedone,
        whodas5_4_housequickly,
        whodas_work,
        
        whodas5_5_daily,
        whodas5_6_workwell,
        whodas5_7_workdone,
        whodas5_8_workquickly,
        
        whodas6_1_community,
        whodas6_2_barriers,
        whodas6_3_dignity,
        whodas6_4_time,
        whodas6_5_emotion,
        whodas6_6_finance,
        whodas6_7_family,
        whodas6_8_relax
      )
    )
  ) == 37
)

completeness_whodas<- "1"
if(!(is.na(data_not_attempted_whodas))){
  if(data_not_attempted_whodas==1)
  {
    completeness_whodas <- "not attempted"}else{}
}else{completeness_whodas<-NA}

if(!(is.na(data_complete_whodas))){
  if(data_complete_whodas==1){
    completeness_whodas <- "complete"} else{}
}else{completeness_whodas<-NA}


if(data_not_attempted_whodas==0 & data_complete_whodas==0){
  completeness_whodas <- "partially completed"}else{}

scores <- data.frame(data_complete_whodas, data_not_attempted_whodas, completeness_whodas )

return(scores)
}


#Calculate summary scores in data 
datwhodas_scored <- adply(datawhodas1, 1, whodas_check)



#________________________________________________________________________________________ 
#Export
#----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/31_WHODAS/WHODAS_scored_data_export.csv", sep="")
write.csv(datwhodas_scored, filename,quote=T,row.names=F,na="#N/A")

print("31_WHODAS_done")


#return completness column
myvars <- c("assessment_id", "completeness_whodas")
newdata <- datwhodas_scored[myvars]
return(newdata)
}



