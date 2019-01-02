#########################################################################################
# Last Date modified: 12/05/2018
# Author: Katy Torres
# Description: Subset of question 12, Demographic: Education, Employment & Income
##########################################################################################

employment <- function(dat0, exportdate)
{
  
#Only retain relevant variables
datemploy <- subset(dat0, 
              select= c(assessment_id,vista_lastname,visit_number,
                        demo_income_group,
                        demo_education,
                        demo_workstatus,
                        demo_hours,
                        demo_occupation,
                        
                        demo_income_none,
                        demo_income_wrk,
                        demo_income_unemp,
                        demo_income_dis,
                        demo_income_gi,
                        demo_income_retire,
                        demo_income_other,
                        demo_income_spec
              ))

#________________________________________________________________________________________              
# Completeness check
#----------------------------------------------------------------------------------------
#Scoring function defined
employ_score <- function(x)
{
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
  
    if(!(is.na(demo_income_none))){  
      if(demo_income_none == 1 | demo_income_wrk  == 1 | demo_income_unemp == 1 |
      demo_income_dis == 1 | demo_income_gi == 1 | demo_income_retire == 1 |
      demo_income_other == 1){income <- 1
      }else{income <- 0}
  }else{income<-0}

#checking for completeness
data_complete_employment<- as.numeric(
  sum(
    is.na(
      c(demo_income_group,
        demo_education,
        demo_workstatus,
        demo_hours,
        demo_occupation, 
        income
      )
    )
  ) == 0
)

data_not_attempted_employment<- as.numeric(
  sum(
    is.na(
      c(demo_income_group,
        demo_education,
        demo_workstatus,
        demo_hours,
        demo_occupation, 
        income
      )
    )
  ) == 5
)

completeness_employment<- "1"
if(!(is.na(data_not_attempted_employment))){
  if(data_not_attempted_employment==1)
  {
    completeness_employment <- "not attempted"}else{}
}else{completeness_employment<-NA}

if(!(is.na(data_complete_employment))){
  if(data_complete_employment==1 & income==1){
    completeness_employment <- "complete"} 
  else if(data_complete_employment==1 & income == 0){
    completeness_employment <- "partially complete"} 
  else if(data_complete_employment==0 & income ==1){
    completeness_employment <- "partially complete"}
  else{}
}else{completeness_employment<-NA}

  if(data_not_attempted_employment==0 & data_complete_employment==0 & income == 0){
    completeness_employment <- "partially completed"}else{}

scores <- data.frame(income, data_not_attempted_employment, data_complete_employment,  completeness_employment)

return(scores)
}

#Calculate summary scores in data 
datemploy_scored <- adply(datemploy, 1, employ_score)

#to anonymize data
datemploy_scored1<- within(datemploy_scored,
                        {
                          assessment_id <- NULL
                          vista_lastname <- NULL
                        })

#________________________________________________________________________________________ 
#Export
#----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/12_DEMO_employment/Demographic_employment_scored_data_export.csv", sep="")
write.csv(datemploy_scored , filename,quote=T,row.names=F,na="#N/A")

filename <- paste("~/Biobank/12_DEMO_employment/Demographic_employment_scored_data_export_DEIDENTIFIED.csv", sep="")
write.csv(datemploy_scored1, filename,quote=T,row.names=F,na="#N/A")
print("12_Employment_done")

#return completness column
myvars <- c("assessment_id", "completeness_employment")
newdata <- datemploy_scored[myvars]
return(newdata)
}

