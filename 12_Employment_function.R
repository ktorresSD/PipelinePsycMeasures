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
  
  specific <- ifelse(!is.na(demo_income_spec),1,0)
  
  
    if(!(is.na(demo_income_none))){  
      if(demo_income_none == 1 | demo_income_wrk  == 1 | demo_income_unemp == 1 |
      demo_income_dis == 1 | demo_income_gi == 1 | demo_income_retire == 1 |
      demo_income_other == 1 | specific == 1){sources_income <- 1
      }else{sources_income <- 0}
  }else{sources_income <-0}

#checking for completeness
data_complete_employment<- as.numeric(
  sum(
    is.na(
      c(demo_income_group,
        demo_education,
        demo_workstatus,
        demo_hours,
        demo_occupation, 
        sources_income
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
        sources_income
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
  if(data_complete_employment==1 & sources_income==1){
    completeness_employment <- "complete"} 
  else if(data_complete_employment==1 & sources_income == 0){
    completeness_employment <- "partially complete"} 
  else if(data_complete_employment==0 & sources_income ==1){
    completeness_employment <- "partially complete"}
  else{}
}else{completeness_employment<-NA}

  if(data_not_attempted_employment==0 & data_complete_employment==0 & sources_income == 0){
    completeness_employment <- "partially completed"}else{}

scores <- data.frame(sources_income, data_not_attempted_employment, data_complete_employment,  completeness_employment)

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
# Descriptive Stats and plots
#----------------------------------------------------------------------------------------

#subset by visit to get report information
v1 <- datemploy_scored[ which(datemploy_scored$visit_number==1), ]
v2 <- datemploy_scored[ which(datemploy_scored$visit_number==2), ]
v3 <- datemploy_scored[ which(datemploy_scored$visit_number==3), ]

#completeness table
table(datemploy_scored$completeness_employment, datemploy_scored$visit_number)


#histograms
par(mfrow=c(2,2))

#What was the total combined income of all members of this family in the past 12 months?
barplot(table(datemploy_scored$demo_income_group), xlab = "Income group", col = c("lightblue"), 
     main = "Frequency of total combined income (all visits)")
# legend("topright", legend = c("1=Less than $15,000",	"2=$15,000-$29,999",	"3=$30,000-$44,999",
# "4=$45,000-$59,999",	"5=$60,000-$74,999",	"6=$75,000-$99,999",	"7=$100,000+"), inset=c(0,0), xpd=TRUE, bty="n"
# )


#What is the highest grade of education that you have completed?
counts <- table(datemploy_scored$demo_education)
barplot(counts, xlab = "Eduaction grade", ylab = "Counts",  col = c("lightblue"),
main = "Counts of Highest grade of Education \n(all visits combined)")
        
        
#Employment Status
t(t((table(datemploy_scored$demo_workstatus))))

#How many hours per week are you currently employed?
hist(datemploy_scored$demo_hours, xlab = "Hours worked", col = c("lightblue"), 
     main = "Frequency of Hours worked per week currently (all visits)")
     
#What are your source(s) of income?
table(datemploy_scored$demo_income_none)
table(datemploy_scored$demo_income_wrk)
table(datemploy_scored$demo_income_unemp)
table(datemploy_scored$demo_income_dis)
table(datemploy_scored$demo_income_gi)
table(datemploy_scored$demo_income_retire)
table(datemploy_scored$demo_income_other)

sort(table(datemploy_scored$demo_income_spec))

#to anonymize data
datemploy_scored1<- within(datemploy_scored,
                      {
                        assessment_id <- NULL
                        vista_lastname <- NULL
                      })


#Wide format

l.sort <- datemploy_scored[order(datemploy_scored$vista_lastname),]
demo_emp_wide <- reshape(l.sort, 
                     timevar = "visit_number",
                     idvar = c( "vista_lastname"),
                     direction = "wide")


#________________________________________________________________________________________ 
#Export
#----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/12_DEMO_employment/Demographic_employment_scored_data_export.csv", sep="")
write.csv(datemploy_scored , filename,quote=T,row.names=F,na="NA")

filename <- paste("~/Biobank/12_DEMO_employment/Demographic_employment_scored_data_export_wide.csv", sep="")
write.csv(demo_emp_wide, filename,quote=T,row.names=F,na="NA")


filename <- paste("~/Biobank/12_DEMO_employment/Demographic_employment_scored_data_export_DEIDENTIFIED.csv", sep="")
write.csv(datemploy_scored1, filename,quote=T,row.names=F,na="NA")
print("12_Employment_done")

#return completness column
myvars <- c("assessment_id", "completeness_employment")
newdata <- datemploy_scored[myvars]
return(newdata)
}

