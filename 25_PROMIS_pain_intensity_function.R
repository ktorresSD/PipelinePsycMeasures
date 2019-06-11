#########################################################################################
# Last Date modified: 06/21/2018
# Author: Katy Torres
# Description: Subset of question 25, PROMIS PAIN INTENSITY
##########################################################################################
promis<- function(dat0, exportdate)
{
  
#Load needed libraries
library(plyr)
library(psych)

#Only retain relevant variables
datpain <- subset(dat0, 
              select= c(assessment_id,vista_lastname,visit_number,
                        pain_level,
                        pain_intensity,
                        pain_average,
                        
                        pain_interfere_life,
                        pain_interfere_conc,
                        pain_interfere_day,
                        pain_interfere_rec,
                        pain_interfere_task,
                        pain_interfere_social

              ))

#________________________________________________________________________________________              
# SCORING Functions Defined
#----------------------------------------------------------------------------------------
pain_scoring <- function(x)
{
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
pain_intensity<- pain_level + pain_intensity + pain_average

pain_interferance<- pain_interfere_life + pain_interfere_conc + pain_interfere_day +
pain_interfere_rec + pain_interfere_task + pain_interfere_social


data_complete_promis <- as.numeric( 
  sum(
    is.na(
      c(pain_level,
        pain_intensity,
        pain_average)
    )
  ) == 0
)

data_not_attempted_promis <- as.numeric( 
  sum(
    is.na(
      c(pain_level,
        pain_intensity,
        pain_average)
    )
  ) == 3
)

if(!(is.na(data_not_attempted_promis))){
  if(data_not_attempted_promis==1)
  {
    completeness_promis <- "not attempted"}else{}
}else{completeness_promis<-NA}

if(!(is.na(data_complete_promis))){
  if(data_complete_promis==1){
    completeness_promis <- "complete"} else{}
}else{completeness_promis<-NA}

if(data_not_attempted_promis==0 & data_complete_promis==0){
  completeness_promis <- "partially completed"}else{}

#return back newly calculated scores

painscores <- data.frame(pain_intensity, pain_interferance, data_complete_promis, data_not_attempted_promis, completeness_promis)
return(painscores)
}


#Calculate summary scores in data
promis_scored <- adply(datpain, 1, pain_scoring)

#to anonymize data
promis_scored1<- within(promis_scored,
                        {
                          assessment_id <- NULL
                          vista_lastname <- NULL
                        })
#________________________________________________________________________________________
# Summary Statistics and plots
#----------------------------------------------------------------------------------------

#completeness table
table(promis_scored$completeness_promis, promis_scored$visit_number)

#subset by visit to get report information
v1 <- promis_scored [ which(promis_scored $visit_number==1), ]
v2 <- promis_scored [ which(promis_scored $visit_number==2), ]
v3 <- promis_scored [ which(promis_scored $visit_number==3), ]


#summary statistics for pain intensity
describe(v1$pain_intensity)
describe(v2$pain_intensity)
describe(v3$pain_intensity)
describe(promis_scored $pain_intensity)

#mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

Mode(v1$pain_intensity)
Mode(v2$pain_intensity)
Mode(v3$pain_intensity)
Mode(promis_scored $pain_intensity)



#summary statistics for pain interferance
describe(v1$pain_interferance)
describe(v2$pain_interferance)
describe(v3$pain_interferance)
describe(promis_scored$pain_interferance)

Mode(v1$pain_interferance)
Mode(v2$pain_interferance)
Mode(v3$pain_interferance)
Mode(promis_scored $pain_interferance)

par(mfrow=c(2,1))
hist(promis_scored$pain_intensity, breaks=10, xlab = "Pain score", ylim=c(0,45), col = c("lightblue"), main = "Pain Intensity scores (all visits)")
hist(promis_scored$pain_interferance, breaks=10, xlab = "Pain score", ylim=c(0,45), col = c("lightblue"), main = "Pain Interference scores (all visits)")

#comparing pain scores
par(mfrow=c(1,1))
plot(promis_scored$pain_interferance, promis_scored$pain_intensity, main= "Pain Intensity vs Pain Interefance scores", 
     xlab= "Pain Interferance scores", ylab= "Pain Intensity Scores", pch= 16, col= "blue")
#correlation between pain scores
cor.test(promis_scored$pain_interferance, promis_scored$pain_intensity, use="pairwise.complete.obs")

abline(lm(promis_scored$pain_intensity~promis_scored$pain_interferance))


#________________________________________________________________________________________ 
#Export
#----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/25_PROMIS_Pain_Intensity/PROMIS_Pain_Intensity_scored_data_export.csv", sep="")
write.csv( promis_scored, filename,quote=T,row.names=F,na="NA")

filename <- paste("~/Biobank/25_PROMIS_Pain_Intensity/PROMIS_Pain_Intensity_scored_data_export_DEIDENTIFIED.csv", sep="")
write.csv( promis_scored1, filename,quote=T,row.names=F,na="NA")


print("25_PROMIS_done")

#return completness column
myvars <- c("assessment_id", "completeness_promis")
newdata <- promis_scored[myvars]
return(newdata)
}



