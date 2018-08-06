#########################################################################################
# Last Date modified: 06/01/2018
# Author: Katy Torres
# Description: Subset of question 8, CDDR and scoring functions
##########################################################################################
cddr <- function(dat0, exportdate)
{
#Load plyr library
library(plyr)

#Only retain relevant variables 
datcddr <- subset(dat0, 
              select= c(assessment_id,vista_lastname,visit_number,
                        CDDR_AgeFirstUse,
                        CDDR_EverSmoked,
                        CDDR_AgeFirstReg,
                        CDDR_RegUse,
                        CDDR_Method,
                        CDDR_Quantity,
                        CDDR_THC,
                        CDDR_LastUse,
                        CDDR_Lifetime,
                        CDDR_Year,
                        CDDR_Month,
                        CDDR_5year,
                        
                        CDDR2_EverSmoked,
                        CDDR2_AgeFirstUse,
                        CDDR2_RegUse,
                        CDDR2_AgeFirstReg,
                        CDDR2_Method,
                        CDDR2_Quantity,
                        CDDR2_THC,
                        CDDR2_LastUse,
                        CDDR2_Lifetime,
                        CDDR2_Year,
                        CDDR2_Month,
                        CDDR2_5year
                        
              ))

#________________________________________________________________________________________              
# Complete variable Defined
#----------------------------------------------------------------------------------------
#Scoring function defined
cddrs_score <- function(x)
{
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
  
if(!(is.na(CDDR_EverSmoked))){
  if(CDDR_EverSmoked==0)
  {
    user <- 0}else{user <- 1}
}else{user<-NA}
  
  
data_complete_cddr1<- as.numeric(
  sum(
    is.na(
      c(CDDR_AgeFirstUse,
        CDDR_EverSmoked,
        CDDR_AgeFirstReg,
        CDDR_RegUse,
        CDDR_Method,
        CDDR_Quantity,
        CDDR_THC,
        CDDR_LastUse,
        CDDR_Lifetime,
        CDDR_Year,
        CDDR_Month,
        CDDR_5year
      )
    )
  ) == 0
)


data_complete_cddr2<- as.numeric(
  sum(
    is.na(
      c(CDDR2_EverSmoked,
        CDDR2_AgeFirstUse,
        CDDR2_RegUse,
        CDDR2_AgeFirstReg,
        CDDR2_Method,
        CDDR2_Quantity,
        CDDR2_THC,
        CDDR2_LastUse,
        CDDR2_Lifetime,
        CDDR2_Year,
        CDDR2_Month,
        CDDR2_5year
      )
    )
  ) == 0
)

if(!(is.na(data_complete_cddr2))){
  if(data_complete_cddr1==1 | data_complete_cddr2==1){
    complete_cddr <-1} else{complete_cddr <-0}
}else{complete_cddr<-NA}



data_not_attempted_cddr1<- as.numeric(
  sum(
    is.na(
      c(CDDR_AgeFirstUse,
        CDDR_EverSmoked,
        CDDR_AgeFirstReg,
        CDDR_RegUse,
        CDDR_Method,
        CDDR_Quantity,
        CDDR_THC,
        CDDR_LastUse,
        CDDR_Lifetime,
        CDDR_Year,
        CDDR_Month,
        CDDR_5year
        
      )
    )
  ) == 12
)


data_not_attempted_cddr2<- as.numeric(
  sum(
    is.na(
      c(CDDR2_EverSmoked,
        CDDR2_AgeFirstUse,
        CDDR2_RegUse,
        CDDR2_AgeFirstReg,
        CDDR2_Method,
        CDDR2_Quantity,
        CDDR2_THC,
        CDDR2_LastUse,
        CDDR2_Lifetime,
        CDDR2_Year,
        CDDR2_Month,
        CDDR2_5year
      )
    )
  ) == 12
)

if(!(is.na(data_not_attempted_cddr2))){
  if(data_not_attempted_cddr1==1 & data_not_attempted_cddr2==1){
    not_attempted_cddr <-1} else{not_attempted_cddr <-0}
}else{not_attempted_cddr<-NA}



completeness_cddr<- "1"
if(!(is.na(not_attempted_cddr))){
  if(not_attempted_cddr==1)
  {
    completeness_cddr <- "not attempted"}else{}
}else{completeness_cddr<-NA}

if(!(is.na(complete_cddr))){
  if(complete_cddr==1){
    completeness_cddr <- "complete"} else{}
}else{completeness_cddr<-NA}

if(not_attempted_cddr==0 & complete_cddr==0){
  completeness_cddr <- "partially completed"}else{}

scores <- data.frame(not_attempted_cddr, complete_cddr, completeness_cddr )

return(scores)
}

#Calculate summary scores in data 
cdddr_scored <- adply(datcddr, 1, cddrs_score)

#to anonymize data
cdddr_scored1<- within(cdddr_scored,
                           {
                             assessment_id <- NULL
                             vista_lastname <- NULL
                           })


#________________________________________________________________________________________ 
#Export
#----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/8_CDDR/CDDR_reduced_data_export.csv", sep="")
write.csv(cdddr_scored , filename,quote=T,row.names=F,na="#N/A")

filename <- paste("~/Biobank/8_CDDR/CDDR_reduced_data_export_DEIDENTIFIED.csv", sep="")
write.csv(cdddr_scored1, filename,quote=T,row.names=F,na="#N/A")

print("8_CDDR_done")

#return completness column
myvars <- c("assessment_id", "completeness_cddr")
newdata <- cdddr_scored [myvars]
return(newdata)
}


