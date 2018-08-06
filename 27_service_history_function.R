#########################################################################################
# Last Date modified: 06/27/2018
# Author: Katy Torres
# Description: Subset of question 27, service history and scoring functions
##########################################################################################

service<- function(dat0, exportdate)
{
  dat<-dat0[dat0$visit_number==1,]
#Only retain relevant variables
datserv <- subset(dat, 
              select= c(assessment_id,vista_lastname,visit_number,
                        serv_oper_none,
                        serv_oper_OEF,
                        serv_oper_OIF,
                        serv_oper_gwot,
                        serv_oper_ond,
                        serv_oper_caribbean,
                        serv_oper_gulf,
                        serv_oper_somalia,
                        serv_oper_bosnia,
                        serv_oper_kosovo,
                        serv_oper_djibouti,
                        serv_oper_libya,
                        Serv_oper_vietnam,
                        serv_oper_korea,
                        serv_oper_other,
                        serv_oper_other1spec,
                        serv_oper_other.1,
                        serv_oper_other2spec,
                        serv,

                        serv_type_0,                    
                        serv_branch_0,                 
                        serv_start_0,                   
                        serv_stop_0,                   
                        serv_discharge_0,               
                        serv_rank_0,                   
                        serv_job_0,  
                        
                        serv_type_1,                   
                        serv_branch_1,                  
                        serv_start_1,                  
                        serv_stop_1,                    
                        serv_discharge_1,              
                        serv_rank_1,                    
                        serv_job_1,
                        serv_count
                        
              ))
#________________________________________________________________________________________              
# SCORING Functions Defined
#----------------------------------------------------------------------------------------
#Scoring function defined
serv_score <- function(x)
{
    for (v in 1:length(x)) assign(names(x)[v], x[[v]])
  
  if(!(is.na(serv_oper_none))){
    if(serv_oper_none==1)
    {
      servedany <- 0}else{servedany <- 1}
  }else{servedany<-NA}
  
  
    #________________________________________________________________________________________              
    # Completeness check
    #----------------------------------------------------------------------------------------
    
    #checking for completeness
    data_about_serv<-as.numeric(
      sum(
        is.na(
          c(serv_count
          )
        )
      ) == 0
    )
    
    data_not_attempted_serv<- as.numeric(
      sum(
        is.na(
          c(serv_oper_none, 
            serv_type_0,
            serv_branch_0,
            serv_start_0,
            serv_stop_0,
            serv_discharge_0,
            serv_rank_0,
            serv_job_0,
            serv_oper_none
          )
        )
      ) == 9
    )
    
    
   completeness_serv<- "1"
   
   if(data_not_attempted_serv==1)
   {
     completeness_serv <- "not_attempted"}else{}
   
   if(!(is.na(servedany))){
     if(servedany==0){
       completeness_serv <- "complete"} else{}
   }else{}

   if(!(is.na(servedany))){
      if(servedany== 1 & data_about_serv==1){
        completeness_serv <- "complete"} else{}
    }else{}

    if(is.na(servedany) & data_about_serv==1){
       completeness_serv <- "partially completed"}else{}
  

  scores <- data.frame(data_not_attempted_serv, completeness_serv )

  return(scores)
}


#Calculate summary scores in data
datserv_scored <- adply(datserv, 1, serv_score)
  
#to anonymize data
datserv_scored1<- within(datserv_scored,
                        {
                          assessment_id <- NULL
                          vista_lastname <- NULL
                        })
#________________________________________________________________________________________ 
#Export
#----------------------------------------------------------------------------------------
filename <- paste("~/Biobank/27_service_history/service_history_scored_data_export.csv", sep="")
write.csv(datserv_scored, filename,quote=T,row.names=F,na="#N/A")

filename <- paste("~/Biobank/27_service_history/service_history_scored_data_export_DEIDENTIFIED.csv", sep="")
write.csv(datserv_scored1, filename,quote=T,row.names=F,na="#N/A")

print("27_service_hist_done")

#return completness column
myvars <- c("assessment_id", "completeness_serv")
newdata <- datserv_scored[myvars]
return(newdata)
}
