#########################################################################################
# Last Date modified: 06/21/2018
# Author: Katy Torres
# Description: Subset of question 24, Positive and Negative Affect Schedule
##########################################################################################
panas<- function(dat0, exportdate)
{
#Load plyr library
library(plyr)
  
#Only retain relevant variables
datpanas <- subset(dat0, select= c(assessment_id,vista_lastname,visit_number,
                        panas1_interest,
                        panas2_distress,
                        panas3_excite,
                        panas4_upset,
                        panas5_strong,
                        
                        panas6_guilt,
                        panas7_scare,
                        panas8_host,
                        panas9_enth,
                        panas10_proud,
                        
                        panas11_irri,
                        panas12_alert,
                        panas13_asham,
                        panas14_insp,
                        panas15_nerv,
                        
                        panas16_deter,
                        panas17_atten,
                        panas18_jitt,
                        panas19_act,
                        panas20_afraid,
                        
                        Panas.Positive,
                        Positive.Affect.Score,
                        Negative.Affect.Score,
                        PANAS.Positive,
                        Liz.s.Formula
                        
              ))

#________________________________________________________________________________________              
# SCORING Functions Defined
#----------------------------------------------------------------------------------------
#Scoring function defined
pan_score <- function(x)
{
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
Positive_Affect_Score <-panas1_interest +
                        panas3_excite +
                        panas5_strong +
                        panas9_enth +
                        panas10_proud +
                        panas12_alert +
                        panas14_insp +
                        panas16_deter +
                        panas17_atten +
                        panas19_act

Negative_Affect_Score <-panas2_distress +
                        panas4_upset +
                        panas6_guilt +
                        panas7_scare +
                        panas8_host +
                        panas11_irri +
                        panas13_asham +
                        panas15_nerv +
                        panas18_jitt +
                        panas20_afraid


data_complete_positive <- as.numeric( 
  sum(
    is.na(
      c(panas1_interest ,
        panas3_excite ,
        panas5_strong ,
        panas9_enth ,
        panas10_proud ,
        panas12_alert ,
        panas14_insp ,
        panas16_deter ,
        panas17_atten ,
        panas19_act)
    )
  ) == 0
)

data_complete_negative <- as.numeric( 
  sum(
    is.na(
      c(panas2_distress,
        panas4_upset,
        panas6_guilt,
        panas7_scare,
        panas8_host,
        panas11_irri,
        panas13_asham,
        panas15_nerv,
        panas18_jitt,
        panas20_afraid)
    )
  ) == 0
)

data_complete_panas <- as.numeric( 
  sum(
    is.na(
      c(panas2_distress,
        panas4_upset,
        panas6_guilt,
        panas7_scare,
        panas8_host,
        panas11_irri,
        panas13_asham,
        panas15_nerv,
        panas18_jitt,
        panas20_afraid,
        panas2_distress,
        panas4_upset,
        panas6_guilt,
        panas7_scare,
        panas8_host,
        panas11_irri,
        panas13_asham,
        panas15_nerv,
        panas18_jitt,
        panas20_afraid)
    )
  ) == 0
)

data_not_attempted_panas <- as.numeric( 
  sum(
    is.na(
      c(panas2_distress,
        panas4_upset,
        panas6_guilt,
        panas7_scare,
        panas8_host,
        panas11_irri,
        panas13_asham,
        panas15_nerv,
        panas18_jitt,
        panas20_afraid,
        panas2_distress,
        panas4_upset,
        panas6_guilt,
        panas7_scare,
        panas8_host,
        panas11_irri,
        panas13_asham,
        panas15_nerv,
        panas18_jitt,
        panas20_afraid)
    )
  ) == 20
)


completeness_panas<- "1"
if(!(is.na(data_not_attempted_panas))){
  if(data_not_attempted_panas==1)
  {
    completeness_panas <- "not attempted"}else{}
}else{completeness_panas<-NA}

if(!(is.na(data_complete_panas))){
  if(data_complete_panas==1){
    completeness_panas <- "complete"} else{}
}else{completeness_panas<-NA}


if(data_not_attempted_panas==0 & data_complete_panas==0 ){
  completeness_panas <- "partially completed"}else{}

scorespanas <- data.frame(Positive_Affect_Score, Negative_Affect_Score, data_complete_positive, data_complete_negative, data_complete_panas, data_not_attempted_panas, completeness_panas)

return(scorespanas)
}


#Calculate summary scores in data 
panas_scores <- adply(datpanas, 1, pan_score)

#to anonymize data
panas_scores1<- within(panas_scores,
                        {
                          assessment_id <- NULL
                          vista_lastname <- NULL
                        })


#________________________________________________________________________________________              
# Descriptive Stats and plots for Report
#----------------------------------------------------------------------------------------

#subset by visit to get report information
v1 <- panas_scores[ which(panas_scores$visit_number==1), ]
v2 <- panas_scores[ which(panas_scores$visit_number==2), ]
v3 <- panas_scores[ which(panas_scores$visit_number==3), ]

#completeness table
table(panas_scores$completeness_panas, panas_scores$visit_number)

#POSITIVE
#summary statistics for total PCL
describe(v1$Positive_Affect_Score)
describe(v2$Positive_Affect_Score)
describe(v3$Positive_Affect_Score)
describe(panas_scores$Positive_Affect_Score)

#mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

Mode(v1$Positive_Affect_Score)
Mode(v2$Positive_Affect_Score)
Mode(v3$Positive_Affect_Score)
Mode(panas_scores$Positive_Affect_Score)

#NEGATIVE
#summary statistics for total PCL
describe(v1$Negative_Affect_Score)
describe(v2$Negative_Affect_Score)
describe(v3$Negative_Affect_Score)
describe(panas_scores$Negative_Affect_Score)

#mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

Mode(v1$Negative_Affect_Score)
Mode(v2$Negative_Affect_Score)
Mode(v3$Negative_Affect_Score)
Mode(panas_scores$Negative_Affect_Score)



#histograms
par(mfrow=c(2,2))
hist(panas_scores$Positive_Affect_Score, breaks=10, xlab = "Positive Affect Score", ylim=c(0,50), col = c("lightyellow"), main = "Positive Affect Score(all visits)")
hist(v1$Positive_Affect_Score, breaks=10, xlab = "Positive Affect Score", ylim=c(0,50), col = c("lightyellow"), main = "Positive Affect Score (visit 1 only)")
hist(v2$Positive_Affect_Score, breaks=10, xlab = "Positive Affect Score", ylim=c(0,50), col = c("lightyellow"), main = "Positive Affect Score (visit 2 only)")
hist(v3$Positive_Affect_Score, breaks=10, xlab = "Positive Affect Score", ylim=c(0,50), col = c("lightyellow"), main = "Positive Affect Score (visit 3 only)")



#histograms
par(mfrow=c(2,2))
hist(panas_scores$Negative_Affect_Score, breaks=10, xlab = "Negative Affect Score", ylim=c(0,50), col = c("lightyellow"), main = "Negative Affect Score(all visits)")
hist(v1$Negative_Affect_Score, breaks=10, xlab = "Negative Affect Score", ylim=c(0,50), col = c("lightyellow"), main = "Negative Affect Score (visit 1 only)")
hist(v2$Negative_Affect_Score, breaks=10, xlab = "Negative Affect Score", ylim=c(0,50), col = c("lightyellow"), main = "Negative Affect Score (visit 2 only)")
hist(v3$Negative_Affect_Score, breaks=10, xlab = "Negative Affect Score", ylim=c(0,50), col = c("lightyellow"), main = "Negative Affect Score (visit 3 only)")



par(mfrow=c(2,1))
hist(panas_scores$Positive_Affect_Score, breaks=10, xlim=c(10,50), xlab = "Positive Affect Scores", col = c("steelblue3"), main = "Histogram for Positive Affect Scores")

hist(panas_scores$Negative_Affect_Score, breaks=10, xlim=c(10,50), xlab = "Negative Affect Scores", col = c("steelblue3"), main = "Histogram for Negative Affect Scores")



#________________________________________________________________________________________ 
#Export
#----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/24_PANAS/PANAS_scored_data_export.csv", sep="")
write.csv(panas_scores, filename,quote=T,row.names=F,na="NA")

filename <- paste("~/Biobank/24_PANAS/PANAS_scored_data_export_DEIDENTIFIED.csv", sep="")
write.csv(panas_scores1, filename,quote=T,row.names=F,na="NA")

print("24_PANAS_done")


#return completness column
myvars <- c("assessment_id", "completeness_panas")
newdata <- panas_scores[myvars]
return(newdata)
}


