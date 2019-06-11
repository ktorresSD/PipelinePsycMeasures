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
  
if(!(is.na(CDDR2_EverSmoked))){
  if(CDDR2_EverSmoked==0)
  {
    user <- 0}else{user <- 1}
}else{user<-NA}
  
 
complete_cddr<- as.numeric(
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


not_attempted_cddr<- as.numeric(
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


completeness_cddr<- "1"
if(!(is.na(not_attempted_cddr))){
  if(not_attempted_cddr==1)
  {
    completeness_cddr <- "not attempted"}else{}
}else{}

if(!(is.na(complete_cddr))){
  if(complete_cddr==1){
    completeness_cddr <- "complete"} else{}
}else{}

if(not_attempted_cddr==0 & complete_cddr==0){
  completeness_cddr <- "partially completed"}else{}

if(!(is.na(CDDR2_EverSmoked))){
  if(CDDR2_EverSmoked== 2){
  completeness_cddr <- "never_smoked_completed"}else{}
}else{}


scores <- data.frame(completeness_cddr )

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

#completeness table
table(cdddr_scored$completeness_cddr, cdddr_scored$visit_number)


#________________________________________________________________________________________ 
#Export
#----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/8_CDDR/CDDR_reduced_data_export.csv", sep="")
write.csv(cdddr_scored , filename,quote=T,row.names=F,na="NA")

filename <- paste("~/Biobank/8_CDDR/CDDR_reduced_data_export_DEIDENTIFIED.csv", sep="")
write.csv(cdddr_scored1, filename,quote=T,row.names=F,na="NA")

print("8_CDDR_done")

#return completness column
myvars <- c("assessment_id", "completeness_cddr")
newdata <- cdddr_scored [myvars]
return(newdata)
}


