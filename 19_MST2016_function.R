#########################################################################################
# Last Date modified: 06/21/2018
# Author: Katy Torres
# Description: Subset of question 19, MST 2016
##########################################################################################
mst<- function(dat0, exportdate)
{

#Only retain relevant variables
datmst <- subset(dat0, 
              select= c(assessment_id,vista_lastname, visit_number,
                        MST_2016_Q1_2,
                        MST_2016_consult
              ))



#________________________________________________________________________________________              
# Completeness Functions Defined
#----------------------------------------------------------------------------------------

mst_score <- function(x)
{
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
  
  if(!(is.na(MST_2016_Q1_2))){
    if(MST_2016_Q1_2==0)
    {
      saidno <- 0}
    else if(MST_2016_Q1_2==2)
    {
      saidno <- 0}else{saidno <- 1}
  }else{saidno<-NA}
  
  #________________________________________________________________________________________              
  # Completeness check
  #----------------------------------------------------------------------------------------
  
  #checking for completeness
  data_complete_mst<- as.numeric(
    sum(
      is.na(
        c(MST_2016_Q1_2,
          MST_2016_consult
        )
      )
    ) == 0
  )
  
  data_not_attempted_mst<- as.numeric(
    sum(
      is.na(
        c(MST_2016_Q1_2,
          MST_2016_consult
        )
      )
    ) == 2
  )
  
  completeness_mst<- "1"
  if(!(is.na(data_not_attempted_mst))){
    if(data_not_attempted_mst==1)
    {
      completeness_mst <- "not attempted"}else{}
  }else{}
  
  if(!(is.na(data_complete_mst))){
    if(data_complete_mst==1){
      completeness_mst <- "complete"} else{}
  }else{}
  
  if(!(is.na(saidno))){
    if(saidno==0){
      completeness_mst <- "complete"} else{}
  }else{}
  
  
  if(data_not_attempted_mst==0 & data_complete_mst==0 & saidno==1){
    completeness_mst <- "partially completed"}else{}
  
  scores <- data.frame( completeness_mst)
  
  return(scores)
}


#Calculate summary scores in data 
datmst_scored <- adply(datmst, 1, mst_score)

  
#________________________________________________________________________________________ 
#Export
#----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/19_MST2016/MST2016_scored_data_export.csv", sep="")
write.csv(datmst_scored , filename,quote=T,row.names=F,na="#N/A")

print("19_mst_done")

#return completness column
myvars <- c("assessment_id", "completeness_mst")
newdata <- datmst_scored [myvars]
return(newdata)
}