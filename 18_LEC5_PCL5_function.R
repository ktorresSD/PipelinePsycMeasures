#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 18,LEC-5/ PCL-5(lifetime)
##########################################################################################

lecpcl<- function(dat0, exportdate)
{
#Only retain relevant variables
datlecpcl <- subset(dat0, select= c(assessment_id,vista_lastname,visit_number,
                              LEC_5_18_MostSevere
              ))


#________________________________________________________________________________________              
# Completeness check
#----------------------------------------------------------------------------------------
lecpcl_score <- function(x)
{
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
#checking for completeness
  
  if(is.na(LEC_5_18_MostSevere)){
    completeness_lecpcl<-"not attempted"
  }else{completeness_lecpcl<-"complete"}


scores <- data.frame(completeness_lecpcl)

return(scores)
}


#Calculate summary scores in data 
datlecpcl_scored <- adply(datlecpcl, 1, lecpcl_score)


#to anonymize data
datlecpcl_scored1<- within(datlecpcl_scored,
                        {
                          assessment_id <- NULL
                          vista_lastname <- NULL
                        })

#________________________________________________________________________________________              
# Descriptive Stats and plots for Report
#----------------------------------------------------------------------------------------

#subset by visit to get report information
v1 <- datlecpcl_scored[ which(datlecpcl_scored$visit_number==1), ]
v2 <- datlecpcl_scored[ which(datlecpcl_scored$visit_number==2), ]
v3 <- datlecpcl_scored[ which(datlecpcl_scored$visit_number==3), ]

#completeness table
table(datlecpcl_scored$completeness_lecpcl, datlecpcl_scored$visit_number)


table(datlecpcl_scored$LEC_5_18_MostSevere)

xx<- barplot(table(datlecpcl_scored$LEC_5_18_MostSevere), 
        col = c("steelblue3"),
        main = "Count of subjects in each response category", 
        ylab = "Subject Count", xlab = "Most severe Event")

table(datlecpcl_scored$LEC_5_18_MostSevere)

  # ## Add text at top of bars
  # text(x = xx, y = dat$freqs, label = datlecpcl_scored$freqs, pos = 3, cex = 0.8, col = "red")

#________________________________________________________________________________________ 
#Export
#----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/18_LEC-5_PCL-5/LEC5_PCL5_scored_data_export.csv", sep="")
write.csv(datlecpcl_scored, filename,quote=T,row.names=F,na="#N/A")

filename <- paste("~/Biobank/18_LEC-5_PCL-5/LEC5_PCL5_scored_data_export_DEIDENTIFIED.csv", sep="")
write.csv(datlecpcl_scored1, filename,quote=T,row.names=F,na="#N/A")

print("18_LEC_done")

#return completness column
myvars <- c("assessment_id", "completeness_lecpcl")
newdata <- datlecpcl_scored[myvars]
return(newdata)
}